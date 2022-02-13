{-|
Module      : JTMS
Description : Justification-based truth maintenance systems (JTMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's justification-based truth
maintenance systems (JTMSes) from Common Lisp to Haskell.

This is not a very \"Haskelly\" implementation; rather, it is a
translation of the original code with minimal changes.  Most of the
deviations from the original are due to either Haskell's strong
typing, which necessitates some additional tagging, and to the
abomination which is Lisp's @do@ macro.  The translation relies on
mutable data structures using `STT` state thread references.  A more
pure translation, possibly not relying on the [@ST@
monad]("Control.Monad.ST")/[@STT@
transformer]("Control.Monad.ST.Trans"), is a significant piece of
future work.

Note also there are restrictions on the embedded monad @m@ which can
be wrapped in the `STT` transformer; see [the @Control.Monad.ST.Trans@
documentation]("Control.Monad.ST.Trans") for details.

See the @LICENSE.txt@ and @README-forbus-dekleer.txt@ files
distributed with this work for a paragraph stating scope of permission
and disclaimer of warranty, and for additional information regarding
copyright ownership.  The above copyright notice and that paragraph
must be included in any separate copy of this file.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, for NON-COMMERCIAL use.  See the License for the specific
language governing permissions and limitations under the License.

-}

{-# LANGUAGE RankNTypes #-}

module Data.TMS.JTMS (
  -- * The JTMST monad
  JTMST, JtmsErr, runJTMST,

  -- * Basic JTMS structures
  JTMS, printJTMS, createJTMS,
  -- ** JTMS accessors
  jtmsTitle,
  -- ** Setting JTMS properties

  -- | === __Lisp origins__
  --
  -- The JTMS property-setting functions are translated from:
  --
  -- > ;; In jtms.lisp:
  -- > (defun change-jtms (jtms &key contradiction-handler node-string
  -- >                            enqueue-procedure debugging
  -- >                               checking-contradictions)
  -- >   (if node-string (setf (jtms-node-string jtms) node-string))
  -- >   (if debugging (setf (jtms-debugging jtms) debugging))
  -- >   (if checking-contradictions
  -- >       (setf (jtms-checking-contradictions jtms)
  -- >          checking-contradictions))
  -- >   (if contradiction-handler
  -- >       (setf (jtms-contradiction-handler jtms) contradiction-handler))
  -- >   (if enqueue-procedure
  -- >       (setf (jtms-enqueue-procedure jtms) enqueue-procedure)))
  setNodeString, nodeStringByDatum, nodeStringByIndex, nodeStringByIndexDatum,
  setDatumString, datumStringByShow,
  setInformantString, informantStringByShow,
  setJustString,
  justStringByIndex, justStringByInformant, justStringByIndexInformant,
  setDebugging, setCheckingContradictions,
  setContradictionHandler, setEnqueueProcedure,

  -- ** Nodes
  Node, createNode, nodeDatum, printTmsNode, assumeNode, nodeString,

  -- *** Accessors for a `Node`'s current state
  getNodeIsAssumption, getNodeIsContradictory, getNodeSupport, getNodeBelieved,
  getNodeConsequences, getNodeInRules, getNodeOutRules, getNodeJusts,

  -- ** Justifications
  Justification, JustRule, printJustRule, justifyNode,

  -- * Reasoning tools
  -- ** Control of assumptions
  enableAssumption, retractAssumption, makeContradiction,
  -- ** Conclusions from current assumption belief
  isInNode, isOutNode, enabledAssumptions, nodeIsPremise, assumptionsOfNode,
  -- ** Output from the current belief state
  whyNodes, whyNode, printContraList,

  -- * Debugging utilities

  -- |Note that these functions are based on `MonadIO`, not just
  -- `Monad`, for printing debugging output.
  debugJTMS, debugNodes, debugNode, debugJusts, debugJust

  ) where

import Control.Monad.State
import Control.Monad.ST.Trans
import Control.Monad.Except
import Control.Monad.Extra
import Data.TMS.Helpers

-- * The @JTMST@ monad transformer
--
-- Construction and manipulation of a JTMS happens inside this monad
-- wrapper.

-- |Errors which can arise from JTMS operations.
data JtmsErr = CannotEnableNonassumption String Int deriving Show

-- |The process of building and using a mutable JTMS.
type JTMSTInner s m a = Monad m => ExceptT JtmsErr (STT s m) a

-- |The process of building and using a mutable JTMS.
newtype Monad m => JTMST s m a = JtmsT { unwrap :: JTMSTInner s m a }

-- |Internal unwrapper preserving rank-2 polymorphism of the state
-- thread in the wrapper `STT`.
unwrap2 :: Monad m => (forall s . JTMST s m a) -> (forall s . JTMSTInner s m a)
unwrap2 (JtmsT m) = m

instance (Monad m) => Functor (JTMST s m) where
  fmap f (JtmsT m) = JtmsT $ do
    v <- m
    return $ f v

instance (Monad m, Functor m) => Applicative (JTMST s m) where
  pure v = JtmsT $ pure v
  (JtmsT m1) <*> (JtmsT m2) = JtmsT $ do
    f <- m1
    v <- m2
    return (f v)

instance (Monad m, Functor m) => Monad (JTMST s m) where
  -- (>>=) :: JTMST s m a -> (a -> JTMST s m b) -> JTMST s m b
  (JtmsT m) >>= f = JtmsT $ m >>= (unwrap . f)

  -- (>>) :: JTMST s m a -> JTMST s m b -> JTMST s m b
  (JtmsT m1) >> (JtmsT m2) = JtmsT $ m1 >> m2

  -- return :: a -> JTMST s m a
  return v = JtmsT $ return v

instance MonadTrans (JTMST s) where
  lift m = JtmsT $ lift $ lift m

instance MonadIO m => MonadIO (JTMST s m) where
  liftIO = lift . liftIO

-- |Lift `STT` behavior to the `JTMST` level.
jLiftSTT :: Monad m => STT s m r -> JTMST s m r
jLiftSTT md = JtmsT $ lift $ md

-- |Lift `ExceptT` behavior to the `JTMST` level.
jLiftExcept :: Monad m => ExceptT JtmsErr (STT s m) r -> JTMST s m r
jLiftExcept md = JtmsT $ md

-- |Execute a computation in the `JTMST` monad transformer.
runJTMST :: Monad m => (forall s . JTMST s m r) -> m (Either JtmsErr r)
runJTMST jtmst = runSTT $ runExceptT $ unwrap2 jtmst

-- * JTMS elements

-- ** The JTMS structure

-- |Standalone implementation of justification-based truth maintenance
-- systems.
--
--  - @d@ is the type of data associated with each `Node` of this JTMS.
--  - @i@ is the type of informants in the external system.
--  - @r@ is the type of rules which may be associated with each `Node`
--    of this JTMS.
--  - @s@ is the (phantom) type of the state thread.
--  - @m@ is the monad in which this computation lives.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defstruct (jtms (:PRINT-FUNCTION print-jtms))
-- >   (title nil)
-- >   (node-counter 0)             ;; unique namer for nodes.
-- >   (just-counter 0)             ;; unique namer for justifications.
-- >   (nodes nil)                  ;; list of all tms nodes.
-- >   (justs nil)                  ;; list of all justifications
-- >   (debugging nil)              ;; debugging flag
-- >   (contradictions nil)         ;; list of contradiction nodes.
-- >   (assumptions nil)            ;; list of assumption nodes.
-- >   (checking-contradictions T)  ;; For external systems
-- >   (node-string nil)
-- >   (contradiction-handler nil)
-- >   (enqueue-procedure nil))
data Monad m => JTMS d i r s m = JTMS {
  -- |Name of this JTMS.
  jtmsTitle :: String,
  -- |Unique namer for nodes.
  jtmsNodeCounter :: STRef s Int,
  -- |Unique namer for justifications.
  jtmsJustCounter :: STRef s Int,
  -- |List of all TMS nodes.
  jtmsNodes :: STRef s [Node d i r s m],
  -- |List of all justifications.
  jtmsJusts :: STRef s [JustRule d i r s m],
  -- |List of all contradiction nodes.
  jtmsContradictions :: STRef s [Node d i r s m],
  -- |List of all assumption nodes.
  jtmsAssumptions :: STRef s [Node d i r s m],
  -- |For external systems.
  jtmsCheckingContradictions :: STRef s Bool,
  jtmsNodeString :: STRef s (Node d i r s m -> String),
  jtmsJustString :: STRef s (JustRule d i r s m -> String),
  jtmsDatumString :: STRef s (d -> String),
  jtmsInformantString :: STRef s (i -> String),
  jtmsEnqueueProcedure :: STRef s (r -> JTMST s m ()),
  jtmsContradictionHandler :: STRef s ([Node d i r s m] -> JTMST s m ()),
  jtmsDebugging :: STRef s Bool
}

-- |TODO For the moment equality on JTMSes in by comparing their names, but
-- this is an ugly and stupid hack.  Need something like: an index for
-- JTMSes generated from JTMST.  Really, this is just for a way to
-- enable nodes from different JTMSes to be seen as unequal.
instance Monad m => Eq (JTMS d i r s m) where
  j1 == j2 = jtmsTitle j1 == jtmsTitle j2


-- |Get the next node counter value, incrementing for future accesses.
nextNodeCounter :: Monad m => JTMS d i r s m -> JTMSTInner s m Int
nextNodeCounter jtms = lift $
  let nodeCounter = jtmsNodeCounter jtms
  in do
    nodeId <- readSTRef nodeCounter
    writeSTRef nodeCounter $ 1 + nodeId
    return nodeId

-- |Get the next justification rule counter value, incrementing for
-- future accesses.
nextJustCounter :: Monad m => JTMS d i r s m -> JTMSTInner s m Int
nextJustCounter jtms = lift $
  let justCounter = jtmsJustCounter jtms
  in do
    justId <- readSTRef justCounter
    writeSTRef justCounter $ 1 + justId
    return justId

-- |Print a simple tag with the title of this JTMS.  Forces the
-- enclosed monad to be `MonadIO`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun print-jtms (jtms stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<JTMS: ~A>" (jtms-title jtms)))
printJTMS jtms = liftIO $ putStr $ "#<JTMS: " ++ jtmsTitle jtms ++ ">"

-- ** Individual nodes

-- |Wrapper for one possible belief known to the TMS.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
-- >   (index 0)
-- >   (datum nil)           ;; pointer to external problem solver
-- >   (label :OUT)          ;; :IN means believed, :OUT means disbelieved
-- >   (support nil)         ;; Current justification or premise marker
-- >   (justs nil)           ;; Possible justifications
-- >   (consequences nil)    ;; Justifications in which it is an antecedent
-- >   (mark nil)            ;; Marker for sweep algorithms
-- >   (contradictory? nil)  ;; Flag marking it as contradictory
-- >   (assumption? nil)     ;; Flag marking it as an assumption.
-- >   (in-rules nil)     ;; Rules that should be triggered when node goes in
-- >   (out-rules nil)    ;; Rules that should be triggered when node goes out
-- >   (jtms nil))           ;; The JTMS in which this node appears.
data Monad m => Node d i r s m = Node {
  nodeIndex :: Int,
  nodeDatum :: d, -- ^Returns the piece of data associated with this node.
  nodeJTMS :: JTMS d i r s m,
  nodeIsAssumption :: STRef s Bool,
  nodeIsContradictory :: STRef s Bool,
  nodeSupport :: STRef s (Maybe (Justification d i r s m)),
  nodeBelieved :: STRef s Bool,
  nodeConsequences :: STRef s [JustRule d i r s m],
  nodeInRules :: STRef s [r],
  nodeOutRules :: STRef s [r],
  nodeJusts :: STRef s [JustRule d i r s m]
}

-- |Equality on `Node`s is based simply on comparing their JTMS and
-- index number.
instance Monad m => Eq (Node d i r s m) where
  n1 == n2 = nodeJTMS n1 == nodeJTMS n2 && nodeIndex n1 == nodeIndex n2

-- |Write one node in the standard way for this JTMS.  Forces the
-- wrapped monad to be `MonadIO`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun print-tms-node (node stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<Node: ~A>" (node-string node)))
printTmsNode :: MonadIO m => Node d i r s m -> JTMST s m ()
printTmsNode node = do
  s <- nodeString node
  liftIO $ putStr $ "#<Node: " ++ s ++ ">"

-- *** Readers of current `Node` state

-- |Return whether a `Node` may currently be used as an assumption.
getNodeIsAssumption :: Monad m => Node d i r s m -> JTMST s m Bool
getNodeIsAssumption = jLiftSTT . readSTRef . nodeIsAssumption

-- |Return whether a `Node` is currently considered a contradiction.
getNodeIsContradictory :: Monad m => Node d i r s m -> JTMST s m Bool
getNodeIsContradictory = jLiftSTT . readSTRef . nodeIsContradictory

-- |Return the current support for believing a `Node`.
getNodeSupport ::
  Monad m => Node d i r s m -> JTMST s m (Maybe (Justification d i r s m))
getNodeSupport = jLiftSTT . readSTRef . nodeSupport

-- |Return whether a `Node` is currently believed.
getNodeBelieved :: Monad m => Node d i r s m -> JTMST s m Bool
getNodeBelieved = jLiftSTT . readSTRef . nodeBelieved

-- |Return the `JustRule`s which use a `Node` as an antecedent.
getNodeConsequences ::
  Monad m => Node d i r s m -> JTMST s m [JustRule d i r s m]
getNodeConsequences = jLiftSTT . readSTRef . nodeConsequences

-- |Return the current in-rules of a `Node`.
getNodeInRules :: Monad m => Node d i r s m -> JTMST s m [r]
getNodeInRules = jLiftSTT . readSTRef . nodeInRules

-- |Return the current out-rules of a `Node`.
getNodeOutRules :: Monad m => Node d i r s m -> JTMST s m [r]
getNodeOutRules = jLiftSTT . readSTRef . nodeOutRules

-- |Return the `JustRule`s which currently give a `Node` as their
-- conclusion.
getNodeJusts :: Monad m => Node d i r s m -> JTMST s m [JustRule d i r s m]
getNodeJusts = jLiftSTT . readSTRef . nodeJusts


-- ** Justifications

-- |Wrapper for one justification relationship between many antecedent
-- nodes and one consequent node.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defstruct (just (:PRINT-FUNCTION print-just))
-- >   (index 0)
-- >   informant
-- >   consequence
-- >   antecedents)
data Monad m => JustRule d i r s m = JustRule {
  justIndex :: Int,
  justInformant :: i,
  justConsequence :: Node d i r s m,
  justAntecedents :: [Node d i r s m]
}

-- |Print the tag of a JTMS justification.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun print-just (just stream ignore)
-- >   (declare (ignore ignore))
-- >   (format stream "#<Just ~D>" (just-index just)))
printJustRule :: MonadIO m => JustRule d i r s m -> JTMST s m ()
printJustRule just =
  liftIO $ putStr $ "#<Just " ++ (show $ justIndex just) ++ ">"

-- |Forms of data which might signal support for a node.  The original
-- Lisp does not need this declaration since it is untyped; the latter
-- two cases are simply symbols.
data Monad m => Justification d i r s m =
  ByRule (JustRule d i r s m) | EnabledAssumption | UserStipulation

-- |Returns @True@ when the node is supported by a `JustRule` with no
-- antecedents.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun tms-node-premise? (node &aux support)
-- >   (and (setq support (tms-node-support node))
-- >        (not (eq support :ENABLED-ASSUMPTION))
-- >        (null (just-antecedents support))))
nodeIsPremise :: Monad m => Node d i r s m -> JTMST s m Bool
nodeIsPremise node = JtmsT $ lift $ do
  support <- readSTRef $ nodeSupport node
  case support of
    Just (ByRule (JustRule _ _ _ antecedents)) -> return $ null antecedents
    _ -> return False

-- * Simple utilities

-- |Produce a representation of the node in the default manner for its
-- JTMS.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun node-string (node)
-- >   (funcall (jtms-node-string (tms-node-jtms node)) node))
nodeString :: Monad m => Node d i r s m -> JTMST s m String
nodeString node = JtmsT $ lift $ do
  ns <- readSTRef $ jtmsNodeString $ nodeJTMS node
  return (ns node)

-- Debugging is turned off for now.

-- -- Overloading the debugging operators to allow printing when the
-- -- underlying monad is `MonadIO`.
-- class Monad m => JTMSDebugger m where
--   debuggingJtms :: String -> m ()
--   debuggingJtms _ = return ()
--
-- instance MonadIO m => JTMSDebugger (JTMST s m) where
--   debuggingJtms s = liftIO $ print s
--
-- -- ===== __Lisp origins:__
-- --
-- -- > ;; In jtms.lisp:
-- -- > (defmacro debugging-jtms (jtms msg &optional node &rest args)
-- -- >   `(when (jtms-debugging ,jtms)
-- -- >      (format *trace-output*
-- -- >        ,msg (if ,node (node-string ,node)) ,@args)))

-- |Raise a JTMS-related error.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun tms-error (string node) (error string (node-string node)))
tmsError :: Monad m => JtmsErr -> JTMST s m ()
tmsError e = JtmsT $ throwError e

-- |The default representation of a node is by @show@ing its datum.
-- Requires that @d@ is in class `Show`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun default-node-string (n) (format nil "~A" (tms-node-datum n)))
defaultNodeString :: (Show d, Monad m) => Node d i r s m -> String
defaultNodeString node = show $ nodeDatum node

-- |Create and return a new JTMS.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun create-jtms (title &key (node-string 'default-node-string)
-- >                                debugging
-- >                                (checking-contradictions t)
-- >                                (contradiction-handler 'ask-user-handler)
-- >                                enqueue-procedure)
-- >   (make-jtms :TITLE title
-- >              :NODE-STRING node-string
-- >              :DEBUGGING debugging
-- >              :CHECKING-CONTRADICTIONS checking-contradictions
-- >              :CONTRADICTION-HANDLER contradiction-handler
-- >              :ENQUEUE-PROCEDURE enqueue-procedure))
createJTMS :: Monad m => String -> JTMST s m (JTMS d i r s m)
createJTMS title = JtmsT $ lift $ do
  nc <- newSTRef 0
  jc <- newSTRef 0
  nodes <- newSTRef ([] :: [Node d i r s m])
  justs <- newSTRef ([] :: [JustRule d i r s m])
  contradictions <- newSTRef ([] :: [Node d i r s m])
  assumptions <- newSTRef ([] :: [Node d i r s m])
  checkingContradictions <- newSTRef True
  nodeString <- newSTRef (show . nodeIndex)
  justString <- newSTRef (show . justIndex)
  datumString <- newSTRef (\ datum -> "?")
  informantString <- newSTRef (\ inf -> "?")
  enqueueProcedure <- newSTRef (\ _ -> return ())
  contradictionHandler <- newSTRef (\ _ -> return ())
  debugging <- newSTRef False
  return (JTMS title nc jc nodes justs contradictions assumptions
               checkingContradictions
               nodeString justString datumString informantString
               enqueueProcedure contradictionHandler debugging)

-- |Helper function for writing setter command for `JTMS` components.
--
-- Not part of the original Lisp.
jtmsSetter :: Monad m =>
  (JTMS d i r s m -> STRef s v) -> JTMS d i r s m -> v -> JTMST s m ()
jtmsSetter field jtms = JtmsT . lift . writeSTRef (field jtms)

-- |Set the display function for `Node`s in a `JTMS`.
--
-- After @change-jtms@ in @jtms.lisp@.
setNodeString ::
  Monad m => JTMS d i r s m -> (Node d i r s m -> String) -> JTMST s m ()
setNodeString = jtmsSetter jtmsNodeString

-- |When the node type @d@ implements `Show`, use this display method
-- as the standard for printing the node.
nodeStringByDatum :: (Monad m, Show d) => JTMS d i r s m -> JTMST s m ()
nodeStringByDatum jtms = setNodeString jtms $ show . nodeDatum

-- |Use the node index for its display.
nodeStringByIndex :: (Monad m) => JTMS d i r s m -> JTMST s m ()
nodeStringByIndex jtms = setNodeString jtms $ show . nodeIndex

-- |When the node type @d@ implements `Show`, use both the node index
-- and this display method as the standard for printing the node.
nodeStringByIndexDatum :: (Monad m, Show d) => JTMS d i r s m -> JTMST s m ()
nodeStringByIndexDatum jtms = setNodeString jtms $ \ n ->
  (show $ nodeIndex n) ++ " " ++ (show $ nodeDatum n)

-- |Set the display function for the datum associated with each `Node`
-- in a `JTMS`.
setDatumString ::
  Monad m => JTMS d i r s m -> (d -> String) -> JTMST s m ()
setDatumString = jtmsSetter jtmsDatumString

-- |When the datum type @d@ implements `Show`, use this display method
-- as the standard for printing the datum.
datumStringByShow :: (Monad m, Show d) => JTMS d i r s m -> JTMST s m ()
datumStringByShow jtms = setDatumString jtms show

-- |Set the display function for informants in a `JTMS`.
setInformantString ::
  Monad m => JTMS d i r s m -> (i -> String) -> JTMST s m ()
setInformantString = jtmsSetter jtmsInformantString

-- |When the informant type @i@ implements `Show`, use this display method
-- as the standard for printing the informant.
informantStringByShow :: (Monad m, Show i) => JTMS d i r s m -> JTMST s m ()
informantStringByShow jtms = setInformantString jtms show

-- |Set the display function for `JustRule`s in a `JTMS`.
--
-- After @change-jtms@ in @jtms.lisp@.
setJustString ::
  Monad m => JTMS d i r s m -> (JustRule d i r s m -> String) -> JTMST s m ()
setJustString = jtmsSetter jtmsJustString

-- |Use the `JustRule` index when printing the just.
justStringByIndex :: (Monad m) => JTMS d i r s m -> JTMST s m ()
justStringByIndex jtms = setJustString jtms $ show . justIndex

-- |When the informant type @i@ implements `Show`, use the `JustRule` index when printing the just.
justStringByInformant :: (Monad m, Show i) => JTMS d i r s m -> JTMST s m ()
justStringByInformant jtms = setJustString jtms $ show . justInformant

-- |When the informant type @i@ implements `Show`, use the `JustRule` index when printing the just.
justStringByIndexInformant ::
  (Monad m, Show i) => JTMS d i r s m -> JTMST s m ()
justStringByIndexInformant jtms = setJustString jtms $ \j ->
  show (justIndex j) ++ " " ++ show (justInformant j)

-- |Turn on or turn off debugging in a JTMS.  This setting currently
-- has no effect.
--
-- After @change-jtms@ in @jtms.lisp@.
setDebugging :: Monad m => JTMS d i r s m -> Bool -> JTMST s m ()
setDebugging = jtmsSetter jtmsDebugging

-- |Set whether the `JTMS` should issue external notifications of
-- contradictions.
--
-- After @change-jtms@ in @jtms.lisp@.
setCheckingContradictions :: Monad m => JTMS d i r s m -> Bool -> JTMST s m ()
setCheckingContradictions = jtmsSetter jtmsCheckingContradictions

-- |Set the contradiction handler.  The `JTMS` default is to do
-- nothing; the intention is to allow a callback to the external
-- system using the `JTMS`.
--
-- After @change-jtms@ in @jtms.lisp@.
setContradictionHandler :: Monad m =>
  JTMS d i r s m -> ([Node d i r s m] -> JTMST s m ()) -> JTMST s m ()
setContradictionHandler = jtmsSetter jtmsContradictionHandler

-- |Set the queuing behavior needed for the external system.
--
-- After @change-jtms@ in @jtms.lisp@.
setEnqueueProcedure :: Monad m =>
  JTMS d i r s m -> (r -> JTMST s m ()) -> JTMST s m ()
setEnqueueProcedure = jtmsSetter jtmsEnqueueProcedure

-- * Basic inference-engine interface

-- |Returns @True@ if the current believed assumptions justify the
-- fact represented by the given node.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun in-node? (node) (eq (tms-node-label node) :IN))
isInNode :: Monad m => Node d i r s m -> JTMST s m Bool
isInNode node = JtmsT $ do
  believed <- lift $ readSTRef (nodeBelieved node)
  return believed

-- |Returns @True@ if the current believed assumptions do not justify
-- the fact represented by the given node.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun out-node? (node) (eq (tms-node-label node) :OUT))
isOutNode :: Monad m => Node d i r s m -> JTMST s m Bool
isOutNode node = do
  believed <- isInNode node
  return $ not believed

-- |Add a node to a JTMS.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun tms-create-node (jtms datum &key assumptionp contradictoryp)
-- >   (let ((node (make-tms-node :INDEX (incf (jtms-node-counter jtms))
-- >                              :DATUM datum
-- >                              :ASSUMPTION? assumptionp
-- >                              :CONTRADICTORY? contradictoryp
-- >                              :JTMS jtms)))
-- >     (if assumptionp (push node (jtms-assumptions jtms)))
-- >     (if contradictoryp (push node (jtms-contradictions jtms)))
-- >     (push node (jtms-nodes jtms))
-- >     node))
createNode :: Monad m => JTMS d i r s m -> d -> Bool -> Bool ->
                           JTMST s m (Node d i r s m)
createNode jtms datum isAssumption isContradictory = JtmsT $ do
  nodeIdx <- nextNodeCounter jtms
  lift $ do
    assumptionRef <- newSTRef isAssumption
    contraRef <- newSTRef isContradictory
    supportRef <- newSTRef Nothing
    believedRef <- newSTRef False
    conseqRef <- newSTRef []
    inRulesRef <- newSTRef []
    outRulesRef <- newSTRef []
    justsRef <- newSTRef []
    let node = Node nodeIdx datum jtms assumptionRef contraRef
                    supportRef believedRef conseqRef
                    inRulesRef outRulesRef justsRef
        nodeListRef = jtmsNodes jtms
      in do
        if isAssumption
          then push node $ jtmsAssumptions jtms
          else return ()

        if isContradictory
          then push node $ jtmsContradictions jtms
          else return ()

        push node nodeListRef

        return node

-- |Internal method used to flag this node as an assumption, and to
-- enable belief in this assumption.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > ;;; Converts a regular node to an assumption and enables it.
-- > (defun assume-node (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (or (tms-node-assumption? node) (tms-node-premise? node))
-- >     (debugging-jtms jtms "~%Converting ~A into an assumption" node)
-- >     (setf (tms-node-assumption? node) t)
-- >     (push node (jtms-assumptions jtms)))
-- >   (enable-assumption node))
assumeNode :: Monad m => Node d i r s m -> JTMST s m ()
assumeNode node =
  let jtms = nodeJTMS node
      isAssumptionRef = nodeIsAssumption node
  in do
    ifM ((notM $ jLiftSTT $ readSTRef isAssumptionRef)
          &&^ (notM $ nodeIsPremise node))
      (do jLiftSTT $ writeSTRef isAssumptionRef True
          jLiftSTT $ push node $ jtmsAssumptions jtms)
      (return ())
    enableAssumption node

-- |API command used when the external system categorizes this node as
-- representing a contradiction.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun make-contradiction (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (tms-node-contradictory? node)
-- >     (setf (tms-node-contradictory? node) t)
-- >     (push node (jtms-contradictions jtms))
-- >     (check-for-contradictions jtms)))
makeContradiction :: Monad m => Node d i r s m -> JTMST s m ()
makeContradiction node =
  let jtms = nodeJTMS node
      isContraRef = nodeIsContradictory node
  in do
    ifM (notM $ jLiftSTT $ readSTRef isContraRef)
      (do jLiftSTT $ writeSTRef isContraRef False
          jLiftSTT $ push node $ jtmsContradictions jtms
          checkForContradictions jtms)
      (return ())

-- |Add a rule for concluding belief in the @consequence@.  The rule
-- is triggered when the @antecedents@ are all believed, and is
-- associated with (perhaps named as) the @informant@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun justify-node (informant consequence antecedents &aux just jtms)
-- >   (setq jtms (tms-node-jtms consequence)
-- >      just (make-just :INDEX (incf (jtms-just-counter jtms))
-- >                      :INFORMANT informant
-- >                      :CONSEQUENCE consequence
-- >                      :ANTECEDENTS antecedents))
-- >   (push just (tms-node-justs consequence))
-- >   (dolist (node antecedents) (push just (tms-node-consequences node)))
-- >   (push just (jtms-justs jtms))
-- >   (debugging-jtms jtms
-- >                "~%Justifying ~A by ~A using ~A."
-- >                consequence
-- >                informant
-- >                (mapcar #'node-string antecedents))
-- >   (if (or antecedents (out-node? consequence))
-- >       (if (check-justification just) (install-support consequence just))
-- >       (setf (tms-node-support consequence) just))
-- >   (check-for-contradictions jtms))
justifyNode :: Monad m =>
                 i -> Node d i r s m -> [Node d i r s m] -> JTMST s m ()
justifyNode informant consequence antecedents =
  let jtms = nodeJTMS consequence
  in do
    justIdx <- JtmsT $ nextJustCounter jtms
    just <- return $ JustRule justIdx informant consequence antecedents

    -- Add this new JustRule as a possible justification of the
    -- consequent.
    jLiftSTT $ push just $ nodeJusts consequence

    -- For each antecedent, add the new rule as a possible consequence
    -- of that antecedent node.
    forM_ antecedents $ \ node -> do
      jLiftSTT $ push just $ nodeConsequences node

    -- Add the new rule to the JTMS's list of justification rules.
    jLiftSTT $ push just $ jtmsJusts jtms

    -- We attempt to use this new rule right now if either the
    -- consequence is currently OUT, or if there actually are
    -- antecedents.
    ifM ((return $ not $ null antecedents)
          ||^ (jLiftSTT $ notM $ readSTRef $ nodeBelieved consequence))
      -- To use the rule now, if the antecedents are satisfied, add it
      -- as a support for the consequence.
      (whenM (checkJustification just) $
       installSupport consequence $ ByRule just)
      -- Otherwise we can install as a support straightaway.
      (jLiftSTT $ writeSTRef (nodeSupport consequence) $ Just $ ByRule just)

    -- Check for new contradictions introduced with this rule.
    checkForContradictions jtms

-- * Support for adding justifications

-- |Detect the case when justification @just@ is satisfied, but the
-- `JTMS` does not believe its consequence.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun check-justification (just)
-- >   (and (out-node? (just-consequence just))
-- >        (justification-satisfied? just)))
checkJustification :: Monad m => JustRule d i r s m -> JTMST s m Bool
checkJustification just =
  (isOutNode $ justConsequence just) &&^ isJustificationSatisfied just

-- |Returns @True@ when all of the antecedents of justification @j@
-- are believed by the `JTMS`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun justification-satisfied? (just)
-- >   (every #'in-node? (just-antecedents just)))
isJustificationSatisfied :: Monad m => JustRule d i r s m -> JTMST s m Bool
isJustificationSatisfied j = allM isInNode $ justAntecedents j

-- |Add a reason for this @conseq@ node to be believed.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun install-support (conseq just)
-- >   (make-node-in conseq just)
-- >   (propagate-inness conseq))
installSupport ::
  Monad m => Node d i r s m -> Justification d i r s m -> JTMST s m ()
installSupport node just = do
  makeNodeIn node just
  propagateInness node

-- |Trigger justifications which rely (directly or indirectly) on the
-- @node@ as an antecedent when @node@ becomes believed.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun propagate-inness (node &aux (jtms (tms-node-jtms node))
-- >                                    (q (list node)))
-- >   (do () ((null (setq node (pop q))))
-- >     (debugging-jtms jtms "~%   Propagating belief in ~A." node)
-- >     (dolist (justification (tms-node-consequences node))
-- >       (when (check-justification justification)
-- >         (make-node-in (just-consequence justification) justification)
-- >         (push (just-consequence justification) q)))))
propagateInness :: Monad m => Node d i r s m -> JTMST s m ()
propagateInness fromNode =
  let jtms = nodeJTMS fromNode
  in do
    queue <- jLiftSTT $ newSTRef [fromNode]
    whileReturnJust (jLiftSTT $ pop queue) $ \ node -> do
      justs <- jLiftSTT $ readSTRef $ nodeConsequences node
      forM_ justs $ \ j ->
        whenM (checkJustification j) $
          let conseq = justConsequence j
          in do
            makeNodeIn conseq $ ByRule j
            jLiftSTT $ push conseq queue

-- |Called when the given @reason@ causes the JTMS to believe @node@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun make-node-in (conseq reason &aux jtms enqueuef)
-- >   (setq jtms (tms-node-jtms conseq)
-- >      enqueuef (jtms-enqueue-procedure jtms))
-- >   (debugging-jtms jtms "~%     Making ~A in via ~A."
-- >           conseq
-- >           (if (symbolp reason)
-- >               reason
-- >               (cons (just-informant reason)
-- >                     (mapcar (jtms-node-string jtms)
-- >                             (just-antecedents reason)))))
-- >   (setf (tms-node-label conseq) :IN)
-- >   (setf (tms-node-support conseq) reason)
-- >   (when enqueuef
-- >     (dolist (in-rule (tms-node-in-rules conseq))
-- >       (funcall enqueuef in-rule))
-- >     (setf (tms-node-in-rules conseq) nil)))
makeNodeIn ::
  Monad m => Node d i r s m -> Justification d i r s m -> JTMST s m ()
makeNodeIn conseq reason =
  let jtms = nodeJTMS conseq
  in do
    enqueuef <- jLiftSTT $ readSTRef $ jtmsEnqueueProcedure jtms
    jLiftSTT $ writeSTRef (nodeBelieved conseq) True
    jLiftSTT $ writeSTRef (nodeSupport conseq) $ Just reason
    forMM_ (jLiftSTT $ readSTRef $ nodeInRules conseq) enqueuef
    jLiftSTT $ writeSTRef (nodeInRules conseq) []

-- > * Assumption Manipulation

-- |This command is called when the external system chooses to
-- disbelieve the assumption represented by @node@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun retract-assumption (node &aux jtms)
-- >   (when (eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >     (setq jtms (tms-node-jtms node))
-- >     (debugging-jtms jtms "~%  Retracting assumption ~A." node)
-- >     (make-node-out node)
-- >     (find-alternative-support jtms
-- >                               (cons node (propagate-outness node jtms)))))
retractAssumption :: Monad m => Node d i r s m -> JTMST s m ()
retractAssumption node = do
  support <- jLiftSTT $ readSTRef (nodeSupport node)
  case support of
    Just EnabledAssumption ->
      let jtms = nodeJTMS node
      in do
        makeNodeOut node
        propagated <- propagateOutness node jtms
        findAlternativeSupport jtms $ node : propagated
    _ -> return ()

isEnabledAssumption :: Monad m => Node d i r s m -> JTMST s m Bool
isEnabledAssumption node = do
  support <- jLiftSTT $ readSTRef $ nodeSupport node
  case support of
    Just EnabledAssumption -> return True
    _ -> return False

supportAntecedents :: Monad m => Node d i r s m -> JTMST s m [Node d i r s m]
supportAntecedents node = do
  support <- jLiftSTT $ readSTRef $ nodeSupport node
  case support of
    Just (ByRule j) -> return $ justAntecedents j
    _ -> return []

emptySupportAntecedents :: Monad m => Node d i r s m -> JTMST s m Bool
emptySupportAntecedents node = do
  ants <- supportAntecedents node
  return $ null ants

-- |Called when the external system chooses to believe the assumption
-- represented by @node@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun enable-assumption (node &aux (jtms (tms-node-jtms node)))
-- >   (unless (tms-node-assumption? node)
-- >     (tms-error "Can't enable the non-assumption ~A" node))
-- >   (debugging-jtms jtms "~%  Enabling assumption ~A." node)
-- >   (cond
-- >      ((out-node? node)
-- >       (make-node-in node :ENABLED-ASSUMPTION)
-- >       (propagate-inness node))
-- >      ((or (eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >           (null (just-antecedents (tms-node-support node)))))
-- >      (t (setf (tms-node-support node) :ENABLED-ASSUMPTION)))
-- >   (check-for-contradictions jtms))
enableAssumption :: Monad m => Node d i r s m -> JTMST s m ()
enableAssumption node =
  let jtms = nodeJTMS node
  in do
    whenM (notM $ jLiftSTT $ readSTRef $ nodeIsAssumption node) $
      jLiftExcept $ throwError $
        CannotEnableNonassumption (jtmsTitle jtms) (nodeIndex node)
    ifM (isOutNode node)
      (do makeNodeIn node EnabledAssumption
          propagateInness node)
      (ifM (isEnabledAssumption node ||^ emptySupportAntecedents node)
        (return ())
        (jLiftSTT $ writeSTRef (nodeSupport node) $ Just EnabledAssumption))
    checkForContradictions jtms

-- |Called when the JTMS disbelieves @node@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun make-node-out (node &aux jtms enqueuef)
-- >   (setq jtms (tms-node-jtms node)
-- >      enqueuef (jtms-enqueue-procedure jtms))
-- >   (debugging-jtms jtms "~%     retracting belief in ~a." node)
-- >   (setf (tms-node-support node) nil)
-- >   (setf (tms-node-label node) :OUT)
-- >   (if enqueuef (dolist (out-rule (tms-node-out-rules node))
-- >               (funcall enqueuef out-rule)))
-- >   (setf (tms-node-out-rules node) nil))
makeNodeOut :: Monad m => Node d i r s m -> JTMST s m ()
makeNodeOut node =
  let jtms = nodeJTMS node
  in do
    enqueuef <- jLiftSTT $ readSTRef $ jtmsEnqueueProcedure jtms
    jLiftSTT $ writeSTRef (nodeSupport node) Nothing
    jLiftSTT $ writeSTRef (nodeBelieved node) False
    forMM_ (jLiftSTT $ readSTRef (nodeOutRules node)) $ \ outRule ->
      enqueuef outRule
    jLiftSTT $ writeSTRef (nodeOutRules node) []

-- |Propagate the retraction of an assumption by finding all other
-- nodes which used that assumption in their justification.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun propagate-outness (node jtms &aux out-queue)
-- >   (debugging-jtms jtms "~%   Propagating disbelief in ~A." node)
-- >   (do ((js (tms-node-consequences node) (append (cdr js) new))
-- >        (new nil nil)
-- >        (conseq nil))
-- >       ((null js) out-queue)
-- >     ;; For each justification using the node, check to see if
-- >     ;; it supports some other node.  If so, forget that node,
-- >     ;; queue up the node to look for other support, and recurse
-- >     (setq conseq (just-consequence (car js)))
-- >     (when (eq (tms-node-support conseq) (car js))
-- >       (make-node-out conseq)
-- >       (push conseq out-queue)
-- >       (setq new (tms-node-consequences conseq)))))
propagateOutness ::
  Monad m => Node d i r s m -> JTMS d i r s m -> JTMST s m [Node d i r s m]
propagateOutness node jtms = error "<TODO unimplemented>"

-- |Search for support for nodes @outs@ which were disbelieved after an
-- assumption retraction.
--
-- The original Lisp code returns the justification when
-- short-circuiting from the inner loop.  But this return value is
-- never used; moreover there is no return value used from callers of
-- this function.  So this type-checked translation returns the unit
-- value.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun find-alternative-support (jtms out-queue)
-- >   (debugging-jtms jtms "~%   Looking for alternative supports.")
-- >   (dolist (node out-queue)
-- >     (unless (in-node? node)
-- >       (dolist (just (tms-node-justs node))
-- >         (when (check-justification just)
-- >           (install-support (just-consequence just) just)
-- >           (return just))))))
findAlternativeSupport ::
  Monad m => JTMS d i r s m -> [Node d i r s m] -> JTMST s m ()
findAlternativeSupport jtms outs = do
  stack <- jLiftSTT $ newSTRef outs
  whileListM_ jLiftSTT stack $ \ node ->
    unlessMM (isInNode node) $
      forMM_ (jLiftSTT $ readSTRef $ nodeJusts node) $ \ just ->
        whenM (checkJustification just) $ do
          installSupport (justConsequence just) (ByRule just)
          propagateInness node

-- > * Contradiction handling interface

-- |Pass all believed contradiction nodes to the
-- @contradictionHandler@.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun check-for-contradictions (jtms &aux contradictions)
-- >   (when (jtms-checking-contradictions jtms)
-- >     (dolist (cnode (jtms-contradictions jtms))
-- >       (if (in-node? cnode) (push cnode contradictions)))
-- >     (if contradictions
-- >       (funcall (jtms-contradiction-handler jtms) jtms contradictions))))
checkForContradictions :: Monad m => JTMS d i r s m -> JTMST s m ()
checkForContradictions jtms = do
  localContras <- jLiftSTT $ newSTRef []
  whenM (jLiftSTT $ readSTRef $ jtmsCheckingContradictions jtms) $ do
    forMM_ (jLiftSTT $ readSTRef $ jtmsContradictions jtms) $ \ cnode ->
      whenM (isInNode cnode) $ (jLiftSTT $ push cnode localContras)
    whenNonnullR jLiftSTT localContras $ \ contras -> do
      handler <- jLiftSTT $ readSTRef $ jtmsContradictionHandler jtms
      handler contras

-- |
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defmacro without-contradiction-check (jtms &body body)
-- >   (contradiction-check jtms nil body))
withoutContradictionCheck ::
  Monad m => JTMS d i r s m -> JTMST s m () -> JTMST s m ()
withoutContradictionCheck jtms = contradictionCheck jtms False

-- |
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defmacro with-contradiction-check (jtms &body body)
-- >   (contradiction-check jtms t body))
withContradictionCheck ::
  Monad m => JTMS d i r s m -> JTMST s m () -> JTMST s m ()
withContradictionCheck jtms = contradictionCheck jtms True

-- |
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun contradiction-check (jtms flag body)
-- >   (let ((jtmsv (gensym)) (old-value (gensym)))
-- >     `(let* ((,jtmsv ,jtms)
-- >          (,old-value (jtms-checking-contradictions ,jtmsv)))
-- >        (unwind-protect
-- >         (progn (setf (jtms-checking-contradictions ,jtmsv) ,flag) ,@body)
-- >       (setf (jtms-checking-contradictions ,jtmsv) ,old-value)))))
contradictionCheck ::
  Monad m => JTMS d i r s m -> Bool -> JTMST s m () -> JTMST s m ()
contradictionCheck jtms flag body = do
  oldFlag <- jLiftSTT $ readSTRef $ jtmsCheckingContradictions jtms
  jLiftSTT $ writeSTRef (jtmsCheckingContradictions jtms) flag
  body
  jLiftSTT $ writeSTRef (jtmsCheckingContradictions jtms) oldFlag

--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defmacro with-contradiction-handler (jtms handler &body body)
-- >   (let ((jtmsv (gensym)) (old-handler (gensym)))
-- >     `(let* ((,jtmsv ,jtms)
-- >          (,old-handler (jtms-contradiction-handler ,jtmsv)))
-- >      (unwind-protect
-- >       (progn (setf (jtms-contradiction-handler ,jtmsv) ,handler) ,@body)
-- >        (setf (jtms-contradiction-handler ,jtmsv) ,old-handler)))))

--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun default-assumptions (jtms)
-- >   (with-contradiction-check jtms
-- >     (with-contradiction-handler jtms #'(lambda (&rest ignore)
-- >                                          (declare (ignore ignore))
-- >                                          (throw 'CONTRADICTION t))
-- >       (dolist (assumption (jtms-assumptions jtms))
-- >         (cond ((eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
-- >                ;; No-op
-- >               )
-- >               ((not (eq :DEFAULT (tms-node-assumption? assumption)))
-- >                ;; No-op
-- >               )
-- >               ((catch 'CONTRADICTION (enable-assumption assumption))
-- >                (retract-assumption assumption)))))))
-- defaultAssumptions :: Monad m => JTMS d i r s m -> JTMST s m ()
-- defaultAssumptions jtms = error "<TODO unimplemented>"

-- > * Well-founded support inqueries

--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun supporting-justification-for-node (node) (tms-node-support node))
supportingJustificationForNode ::
  Monad m => Node d i r s m -> JTMST s m (Maybe (Justification d i r s m))
supportingJustificationForNode node = jLiftSTT $ readSTRef $ nodeSupport node

-- |API command returning the believed assumption nodes used to
-- justify belief in this node.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun assumptions-of-node (node &aux assumptions (marker (list :MARK)))
-- >   (do ((nodes (list node) (append (cdr nodes) new))
-- >        (new nil nil))
-- >       ((null nodes) assumptions)
-- >     (let ((node (car nodes)))
-- >       (cond
-- >         ((eq (tms-node-mark node) marker))
-- >         ((eq (tms-node-support node) :ENABLED-ASSUMPTION)
-- >          (push node assumptions))
-- >         ((in-node? node)
-- >          (setq new (just-antecedents (tms-node-support node)))))
-- >       (setf (tms-node-mark node) marker))))
assumptionsOfNode :: Monad m => Node d i r s m -> JTMST s m [Node d i r s m]
assumptionsOfNode node =
  let jtms = nodeJTMS node
  in do
    nodes <- jLiftSTT $ readSTRef $ jtmsNodes jtms

    -- We look at each node at most once.
    marking <- jLiftSTT $ newSTArray (0, length nodes - 1) False

    -- Set up a list for results.
    assumptions <- jLiftSTT $ newSTRef []

    -- Set up the stack of nodes to consider.
    queue <- jLiftSTT $ newSTRef []
    jLiftSTT $ push node queue

    -- Loop while the stack is not empty.
    whileListM_ jLiftSTT queue $ \node ->
      let idx = nodeIndex node
      in do
        -- Make sure we do not process a node more than once.
        unlessMM (jLiftSTT $ readSTArray marking idx) $ do
          -- The case when the node is an enabled assumption
          ifM (isEnabledAssumption node)
            (jLiftSTT $ push node assumptions)

            -- The alternative case where the node is believed
            (whenM (jLiftSTT $ readSTRef $ nodeBelieved node) $ do
                support <- getNodeSupport node
                case support of
                  Just (ByRule j) ->
                    jLiftSTT $ pushAll (justAntecedents j) queue
                  _ -> return ())

          jLiftSTT $ writeSTArray marking idx True

    -- The result is assumptions we've accumulated.
    jLiftSTT $ readSTRef assumptions

    
-- |Returns the list of currently enabled assumptions.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun enabled-assumptions (jtms &aux result)
-- >   (dolist (assumption (jtms-assumptions jtms) result)
-- >     (if (eq (tms-node-support assumption) :ENABLED-ASSUMPTION)
-- >      (push assumption result))))
enabledAssumptions :: Monad m => JTMS d i r s m -> JTMST s m [Node d i r s m]
enabledAssumptions jtms = error "<TODO unimplemented>"

-- > * Inference engine stub to allow this JTMS to be used standalone

-- |Print the belief state and any justification of this node.
-- Requires that the underlying monad @m@ be `MonadIO`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun why-node (node &aux justification)
-- >   (setq justification (tms-node-support node))
-- >   (cond ((eq justification :ENABLED-ASSUMPTION)
-- >       (format t "~%~A is an enabled assumption"
-- >               (node-string node)))
-- >      (justification
-- >       (format t "~%~A is IN via ~A on"
-- >               (node-string node)
-- >               (just-informant justification))
-- >       (dolist (anode (just-antecedents justification))
-- >         (format t "~%  ~A" (node-string anode))))
-- >      (T (format t "~%~A is OUT." (node-string node))))
-- >   node)
whyNode :: MonadIO m => Node d i r s m -> JTMST s m ()
whyNode node = error "<TODO unimplemented>"

-- |Prints the justifications of all current nodes.  Requires that the
-- underlying monad @m@ be `MonadIO`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun why-nodes (jtms)
-- >   (dolist (node (jtms-nodes jtms)) (why-node node)))
whyNodes :: MonadIO m => JTMS d i r s m -> JTMST s m ()
whyNodes jtms = error "<TODO unimplemented>"

-- |
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (proclaim '(special *contra-assumptions*))
-- >
-- > (defun ask-user-handler (jtms contradictions)
-- >   (handle-one-contradiction (car contradictions))
-- >   (check-for-contradictions jtms))
askUserHandler ::
  MonadIO m => JTMS d i r s m -> [Node d i r s m] -> JTMST s m ()
askUserHandler jtms contradictions = error "<TODO unimplemented>"

-- |
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun handle-one-contradiction (contra-node
-- >                               &aux the-answer *contra-assumptions*)
-- >   (setq *contra-assumptions* (assumptions-of-node contra-node))
-- >   (unless *contra-assumptions*
-- >     (tms-error "~%There is a flaw in the universe...~A" contra-node))
-- >   (format t "~%Contradiction found: ~A" (node-string contra-node))
-- >   (print-contra-list *contra-assumptions*)
-- >   (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
-- >   (setq the-answer
-- >      (catch 'tms-contradiction-handler
-- >        (break "JTMS contradiction break")))
-- >   (if (and (integerp the-answer)
-- >         (> the-answer 0)
-- >         (not (> the-answer (length *contra-assumptions*))))
-- >       (retract-assumption (nth (1- the-answer)
-- >                             *contra-assumptions*))))
handleOneContradiction :: Monad m => JTMS d i r s m -> JTMST s m ()
handleOneContradiction node = error "<TODO unimplemented>"

-- |Print a verbose debugging output list of the contradictions in the
-- JTMS.  Requires that the underlying monad @m@ be `MonadIO`.
--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun print-contra-list (nodes)
-- >   (do ((counter 1 (1+ counter))
-- >        (nn nodes (cdr nn)))
-- >       ((null nn))
-- >     (format t "~%~A ~A" counter
-- >          (node-string (car nn)))))
printContraList :: MonadIO m => [Node d i r s m] -> JTMST s m ()
printContraList nodes = error "<TODO unimplemented>"

-- |
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- > (defun tms-answer (num)
-- >   (if (integerp num)
-- >       (if (> num 0)
-- >        (if (not (> num (length *contra-assumptions*)))
-- >            (throw 'tms-contradiction-handler num)
-- >            (format t "~%Ignoring answer, too big."))
-- >        (format t "~%Ignoring answer, too small"))
-- >       (format t "~%Ignoring answer, must be an integer.")))
tmsAnswer :: MonadIO m => Int -> JTMST s m ()
tmsAnswer = error "<TODO unimplemented>"

-- |Print debugging information about a `JTMS`.
debugJTMS :: MonadIO m => String -> JTMS d i r s m -> JTMST s m ()
debugJTMS desc jtms = do
  liftIO $ putStrLn $ "----- " ++ desc
  debugJusts jtms
  debugNodes jtms
  liftIO $ putStrLn "-----"

-- |Print debugging information about the `Node`s of a `JTMS`.
debugNodes :: MonadIO m => JTMS d i r s m -> JTMST s m ()
debugNodes jtms = forMM_ (jLiftSTT $ readSTRef $ jtmsNodes jtms) $
  \ node -> debugNode node

-- |Print debugging information about a `Node`.
debugNode :: MonadIO m => Node d i r s m -> JTMST s m ()
debugNode node = let jtms = nodeJTMS node
  in do
    nodeFmt <- jLiftSTT $ readSTRef $ jtmsNodeString $ jtms
    justFmt <- jLiftSTT $ readSTRef $ jtmsJustString $ jtms
    datumFmt <- jLiftSTT $ readSTRef $ jtmsDatumString $ nodeJTMS node
    informantFmt <- jLiftSTT $ readSTRef $ jtmsInformantString $ nodeJTMS node
    isAssumption <- jLiftSTT $ readSTRef $ nodeIsAssumption node
    isContradictory <- jLiftSTT $ readSTRef $ nodeIsContradictory node
    support <- jLiftSTT $ readSTRef $ nodeSupport node
    believed <- jLiftSTT $ readSTRef $ nodeBelieved node
    justs <- jLiftSTT $ readSTRef $ nodeJusts node
    consequences <- jLiftSTT $ readSTRef $ nodeConsequences node

    liftIO $ do
      putStrLn $ "Node " ++ (show $ nodeIndex node)
        ++ " [" ++ (nodeFmt node) ++ "] "
        ++ "(isAssumption " ++ show isAssumption
        ++ ", isContradictory " ++ show isContradictory ++ ", "
        ++ (if believed then "" else "not ") ++ "believed)"

      case support of
        Just EnabledAssumption -> putStrLn "- Supported: enabled assumption"
        Just UserStipulation   -> putStrLn "- Supported: user stipulation"
        Just (ByRule j) ->
          putStrLn $ "- IN via {j.informant} (" ++ (justFmt j) ++ ")"
        Nothing -> putStrLn "- OUT"

      if null justs
        then putStrLn "- Concluded by no justification rules"
        else do
          putStrLn $ "- Concluded by " ++ (show $ length justs)
            ++ " justification rule" ++ (if length justs == 1 then "" else "s")
            ++ ": " ++ (foldl1 (\ x y -> x ++ ", " ++ y) $ map justFmt justs)

      if null consequences
        then putStrLn "- Antecedent to no rules"
        else do
          putStrLn $ "- Antecedent to " ++ (show $ length consequences)
            ++ " justification rule"
            ++ (if length consequences == 1 then "" else "s")
            ++ ": " ++ commaList justFmt consequences

-- |Print debugging information about the `JustRule`s of a `JTMS`.
debugJusts :: MonadIO m => JTMS d i r s m -> JTMST s m ()
debugJusts jtms = forMM_ (jLiftSTT $ readSTRef $ jtmsJusts jtms) $
  \ just -> debugJust jtms just

-- |Print debugging information about a `JustRule`.
debugJust :: MonadIO m => JTMS d i r s m -> JustRule d i r s m -> JTMST s m ()
debugJust jtms just = do
  nodeFmt <- jLiftSTT $ readSTRef $ jtmsNodeString jtms
  justFmt <- jLiftSTT $ readSTRef $ jtmsJustString jtms
  nodeFmt <- jLiftSTT $ readSTRef $ jtmsNodeString jtms
  liftIO $ putStrLn $
    "JustRule (" ++ (justFmt just) ++ ") "
      ++ (nodeFmt $ justConsequence just) ++ " <= "
      ++ (commaList nodeFmt $ justAntecedents just)

--
-- ===== __Lisp origins:__
--
-- > ;; In jtms.lisp:
-- >
-- > (defun explore-network (node)
-- >   (unless (in-node? node)
-- >        (format t "~% Sorry, ~A not believed." (node-string node))
-- >        (return-from explore-network node))
-- >   (do ((stack nil)
-- >        (current node)
-- >        (options nil)
-- >        (olen 0)
-- >        (done? nil))
-- >       (done? current)
-- >       (why-node current)
-- >       (setq options (if (typep (tms-node-support current) 'just)
-- >                      (just-antecedents (tms-node-support current))))
-- >       (setq olen (length options))
-- >       (do ((good? nil)
-- >         (choice 0))
-- >        (good? (case good?
-- >                     (q (return-from explore-network current))
-- >                     (0 (if stack
-- >                            (setq current (pop stack))
-- >                            (return-from explore-network current)))
-- >                     (t (push current stack)
-- >                        (setq current (nth (1- good?) options)))))
-- >        (format t "~%>>>")
-- >        (setq choice (read))
-- >        (cond ((or (eq choice 'q)
-- >                   (and (integerp choice)
-- >                        (not (> choice olen))
-- >                        (not (< choice 0))))
-- >               (setq good? choice))
-- >              (t (format t
-- >                  "~% Must be q or an integer from 0 to ~D."
-- >                  olen))))))

-- * Other helpers

-- |This instance declaration is not part of `STT`, but it is
-- convenient.
instance MonadIO m => MonadIO (STT s m) where
  liftIO = lift . liftIO
