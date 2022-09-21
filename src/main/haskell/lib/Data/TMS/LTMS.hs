{-|
Module      : LTMS
Description : Logic-based truth maintenance systems (LTMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's logic-based truth
maintenance systems (LTMSs) from Common Lisp to Haskell.

This is not a very \"Haskelly\" implementation; rather, it is a
translation of the original code with minimal changes.  Most of the
deviations from the original are due to either Haskell's strong
typing, which necessitates some additional tagging, and to the
abomination which is Lisp's @do@ macro.  The translation relies on
mutable data structures using `STT` state thread references.  A more
pure translation, possibly not relying on the
[@ST@ monad]("Control.Monad.ST")/[@STT@ transformer]("Control.Monad.ST.Trans"),
is a significant piece of future work.

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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.LTMS (
  -- * The LTMST monad
  LTMST,
  LtmsErr(DuplicatedDatum, CannotEnableNonassumption, RedundantNodeEnable,
          NullClause, UnknownNode, GlobalContradiction, TotalContradiction,
          FromMonadFail),
  runLTMST,

  -- * LTMS data structures

  -- ** Component classes
  NodeDatum, contradictionNodeDatum,

  -- ** Top-level LTMS
  LTMS,

  -- *** LTMS components
  getLTMSMutable, setLTMSMutable,
  getNodes, getClauses,
  getDebugging, setDebugging,
  getCheckingContradictions, setCheckingContradictions,
  getNodeString, setNodeString,
  getPendingContradictions, getComplete,
  setComplete,
  getViolatedClauses,
  getDelaySat, setDelaySat,
  getDatumString, setDatumString,
  setDatumStringViaString, setDatumStringViaShow,
  getInformantString, setInformantString,
  setInformantStringViaString, setInformantStringViaShow,
  getEnqueueProcedure, setEnqueueProcedure,
  nextNodeCounter, nextClauseCounter,

  -- ** Nodes
  Node,
  -- *** Node components
  -- *** Setting node status

  -- ** Clauses
  Clause,
  -- *** Clause components
  -- *** Setting clause status

  -- ** Justifications

  -- ** Environments and tables

  -- * Deduction and search utilities

  -- ** Related to a node

  -- ** Related to environments

  -- * Printing and debugging

  -- | In addition to the functions documented below, many of the
  -- types defined in this module are instances of the classes defined
  -- in `Data.TMS.Formatters`.  Specifically:
  --
  --  - __Defined for `format`, `blurb`, etc:__
  --
  --  - __Defined for `pprint`:__
  --    `Env`, `EnvTable`
  --
  --  - __Defined for `debug`:__
  --
  -- Functions prefixed @format@ build a computation returning a
  -- `String`.  Functions prefixed @debug@ or @print@ build a unit
  -- computation printing the artifact in question to standard output;
  -- those with prefix @debug@ are generally more verbose.

  -- ** Nodes and node lists

  -- ** Environments, labels, and tables

  -- * TODO
  printLtms,
  printTmsNode,
  printClause,
  nodeString,
  ltmsError,
  defaultNodeString,
  isSatisfiedClause,
  isViolatedClause,
  createLtms,
  changeLtms,
  isUnknownNode,
  isKnownNode,
  isTrueNode,
  isFalseNode,
  tmsCreateNode,
  enableAssumption,
  convertToAssumption,
  retractAssumption,
  addFormula,
  simplifyClause,
  sortClause,
  normalize,
  normalize1,
  normalizeTax,
  normalizeConjunction,
  normalizeIff,
  normalizeDisjunction,
  disjoin,
  findNode,
  walkClauses,
  generateCode,
  partial,
  addClause,
  addClauseInternal,
  bcpAddClause,
  insertTrueClause,
  insertFalseClause,
  addNogood,
  checkClauses,
  checkClause,
  findUnknownPair,
  topSetTruth,
  setTruth,
  propagateUnknownness,
  clauseConsequent,
  findAlternativeSupport,
  checkForContradictions,
  contradictionHandler,
  withoutContradictionCheck,
  withContradictionCheck,
  contradictionCheck,
  withContradictionHandler,
  withAssumptions,
  supportForNode,
  assumptionsOfNode,
  assumptionsOfClause,
  askUserHandler,
  handleOneContradiction,
  printContraList,
  tmsAnswer,
  avoidAll,
  clauseAntecedents,
  signedNodeString,
  nodeConsequences,
  whyNode,
  whyNodes,
  explainNode,
  explain1,
  prettyPrintClauses,
  prettyPrintClause,
  showNodeConsequences,
  nodeShowClauses,
  exploreNetwork
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Trans
import Control.Monad.Trans.Except
import Control.Monad.Extra
import Data.List
import Data.Symbol
import Data.TMS.Formatters
import Data.TMS.Helpers
import Data.TMS.MList
import Data.TMS.Dbg

-- import OptionsTH
import Data.TMS.TH


-- * The @LTMST@ monad transformer
--
-- Construction and manipulation of a LTMS happens inside this
-- wrapper.

-- |Errors which can arise from LTMS operations.
data LtmsErr = DuplicatedDatum String Int
               -- ^ Thrown when a datum is duplicated between
               -- different nodes.  The `String` and `Int`eger values
               -- are of the original node.
             | CannotEnableNonassumption String Int
               -- ^ Thrown when an attempt is made to enable a `Node`
               -- which is not an assumption.  The `String` and
               -- `Int`eger values are of the relevant node.
             | RedundantNodeEnable String Int
               -- ^ Thrown when an attempt is made to enable a `Node`
               -- which is already enabled.  The `String` and
               -- `Int`eger values are of the relevant node.
             | NullClause String
               -- ^ Thrown when a null (thus contradictory) clause is
               -- added.  The `String` refers to the
               -- ZZZZZZZZZZZZZZZZZZZZZZ FILL IN.
             | UnknownNode
               -- ^ Thrown from `assumptionOfClause`.
             | GlobalContradiction
               -- ^ Thrown from `handleOneContradiction`.
             | TotalContradiction
               -- ^ Thrown from `avoidAll`.
             | FromMonadFail String
               -- ^ Indicates a pattern-matching failure within an
               -- `LTMST` operation, or other described failure.
  deriving Show

{- ===== Internal state of an LTMST. =================================== -}

-- |Internal state of an LTMST process
data LtmstState = LtmstState {
  -- initialEnvTableAlloc :: Int,
  -- envTableIncr :: Int
  }

-- |Initial state of an LTMST process.
initialLtmstState :: LtmstState
initialLtmstState = LtmstState -- 50 75

{-
-- |Update the initial table size of an LTMST state.
withInitialEnvTableAlloc :: LtmstState -> Int -> LtmstState
withInitialEnvTableAlloc (LtmstState _ ei) ia = LtmstState ia ei

-- |Update the table increment size of an LTMST state.
withEnvTableIncr :: LtmstState -> Int -> LtmstState
withEnvTableIncr (LtmstState ia _) ei = LtmstState ia ei
-}

{- ===== LTMST definition. ============================================= -}

-- |The process of building and using a mutable LTMS.
type LTMSTInner s m a =
  Monad m => ExceptT LtmsErr (StateT LtmstState (STT s m)) a

-- |The process of building and using a mutable LTMS.
newtype Monad m => LTMST s m a = LtmsT { unwrap :: LTMSTInner s m a }

-- |Internal unwrapper preserving rank-2 polymorphism of the state
-- thread in the wrapper `STT`.
unwrap2 :: Monad m => (forall s . LTMST s m a) -> (forall s . LTMSTInner s m a)
unwrap2 (LtmsT m) = m

instance (Monad m) => Functor (LTMST s m) where
  fmap f (LtmsT m) = LtmsT $ do
    v <- m
    return $ f v

instance (Monad m, Functor m) => Applicative (LTMST s m) where
  pure v = LtmsT $ pure v
  (LtmsT m1) <*> (LtmsT m2) = LtmsT $ do
    f <- m1
    v <- m2
    return (f v)

instance (Monad m, Functor m) => Monad (LTMST s m) where
  -- (>>=) :: LTMST s m a -> (a -> LTMST s m b) -> LTMST s m b
  (LtmsT m) >>= f = LtmsT $ m >>= (unwrap . f)

  -- (>>) :: LTMST s m a -> LTMST s m b -> LTMST s m b
  (LtmsT m1) >> (LtmsT m2) = LtmsT $ m1 >> m2

  -- return :: a -> LTMST s m a
  return v = LtmsT $ return v

instance MonadTrans (LTMST s) where
  lift m = LtmsT $ lift $ lift $ lift m

instance MonadIO m => MonadIO (LTMST s m) where
  liftIO = lift . liftIO

-- |Lift `STT` behavior to the `LTMST` level.
sttLayer :: Monad m => STT s m r -> LTMST s m r
sttLayer md = LtmsT $ lift $ lift $ md

-- |Lift `ExceptT` behavior to the `LTMST` level.
exceptLayer ::
  Monad m => ExceptT LtmsErr (StateT LtmstState (STT s m)) r -> LTMST s m r
exceptLayer = LtmsT

-- |Lift `StateT` behavior to the `LTMST` level.
stateLayer ::
  Monad m => StateT LtmstState (STT s m) r -> LTMST s m r
stateLayer = LtmsT . lift

instance Monad m => MonadFail (LTMST s m) where
  fail s = exceptLayer $ throwE $ FromMonadFail s

{-
-- |Retrieve the current initial `Env` table size setting.
getInitialEnvTableAlloc :: Monad m => LTMST s m Int
getInitialEnvTableAlloc = stateLayer $ fmap initialEnvTableAlloc get

-- |Retrieve the current initial `Env` table size setting.
setInitialEnvTableAlloc :: Monad m => Int -> LTMST s m ()
setInitialEnvTableAlloc ia = stateLayer $ modify (`withInitialEnvTableAlloc` ia)

-- |Retrieve the current initial `Env` table size setting.
getEnvTableIncr :: Monad m => LTMST s m Int
getEnvTableIncr = stateLayer $ fmap envTableIncr get

-- |Retrieve the current initial `Env` table size setting.
setEnvTableIncr :: Monad m => Int -> LTMST s m ()
setEnvTableIncr ia = stateLayer $ modify (`withEnvTableIncr` ia)
-}

-- |Execute a computation in the `LTMST` monad transformer.
runLTMST :: Monad m => (forall s . LTMST s m r) -> m (Either LtmsErr r)
runLTMST ltmst = do
  let core = unwrap2 ltmst
      afterExcept = runExceptT core
      afterState = do
        (result, endState) <- runStateT afterExcept initialLtmstState
        return result
  runSTT afterState

{- ===== LTMS structure types. ============================================ -}

-- |Class of type which can be used as the datum underlying `Node`s in
-- an `LTMS`.
class NodeDatum d where
  -- |The datum associated with the contradiction node in a
  -- newly-initialized `LTMS` with `Node` data of this type.
  contradictionNodeDatum :: d

instance NodeDatum String where
  contradictionNodeDatum = "The contradiction"
instance NodeDatum Symbol where
  contradictionNodeDatum = intern "The contradiction"

-- | Top-level representation of a logic-based truth maintenance
-- system.
--
-- > (defstruct (ltms (:PRINT-FUNCTION print-ltms))
-- >   (title nil)
-- >   (node-counter 0)              ; unique namer for nodes.
-- >   (clause-counter 0)            ; unique namer for justifications.
-- >   (nodes nil)                   ; hash table for nodes.
-- >   (clauses nil)                 ; list of all clauses.
-- >   (debugging nil)               ; debugging flag
-- >   (checking-contradictions t)
-- >   (node-string nil)
-- >   (contradiction-handlers nil)
-- >   (pending-contradictions nil)
-- >   (enqueue-procedure nil)
-- >   (complete nil)                ; Is this a complete LTMS?
-- >   (violated-clauses nil)
-- >   (queue nil)                      ; Queue of clauses to resolve.
-- >   (conses nil)                     ; Source of conses to reuse in inner loop.
-- >   (delay-sat nil)          ; Don't resolve satisfied clauses.
-- >   (cons-size 0))           ; Size of temporary structure.
data (Monad m, NodeDatum d) => LTMS d i r s m = LTMS {
  -- |Name of this LTMS.
  ltmsTitle :: String,
  -- |Unique namer for nodes.
  ltmsNodeCounter :: STRef s Int,
  -- |Unique namer for clauses.
  ltmsClauseCounter :: STRef s Int,
  -- |List of all TMS nodes.
  ltmsNodes :: STRef s [Node d i r s m],
  -- |List of all clauses.
  ltmsClauses :: STRef s [Clause d i r s m],
  -- |Set to `True` when we wish to debug this LTMS.
  ltmsDebugging :: STRef s Bool,
  -- |Unique namer for clauses.
  ltmsCheckingContradictions :: STRef s Bool,
  -- |Function for formatting a `Node` of this LTMS.
  ltmsNodeString :: STRef s (Node d i r s m -> String),
  -- |
  ltmsPendingContradictions :: STRef s [Node d i r s m],
  -- |List of external procedures to be executed for this LTMS.
  ltmsEnqueueProcedure :: STRef s (r -> LTMST s m ()),
  -- |Set to `True` when this is a complete LTMS.
  ltmsComplete :: STRef s Bool,
  -- |List of violated clauses.
  ltmsViolatedClauses :: STRef s [Clause d i r s m],
  -- |Queue of clauses to resolve.
  ltmsQueue :: STRef s [Clause d i r s m],
  -- |Don't resolve satisfied clauses
  ltmsDelaySat :: STRef s Bool,
  -- |Function for representing the data associated with `Node`s.
  ltmsDatumString :: STRef s (d -> String),
  -- |Function for representing the informants of justifications.
  ltmsInformantString :: STRef s (i -> String)
  }

-- |Shortcut maker for reading from an `LTMS` reference.
getLTMSMutable ::
  (Monad m, NodeDatum d) =>
    (LTMS d i r s m -> STRef s a) -> LTMS d i r s m  -> LTMST s m a
{-# INLINE getLTMSMutable #-}
getLTMSMutable refGetter ltms = sttLayer $ readSTRef (refGetter ltms)
-- |Shortcut to write to an LTMS reference.
setLTMSMutable ::
  (Monad m, NodeDatum d) =>
    (LTMS d i r s m -> STRef s a) -> LTMS d i r s m -> a -> LTMST s m ()
{-# INLINE setLTMSMutable #-}
setLTMSMutable refGetter ltms envs =
  sttLayer $ writeSTRef (refGetter ltms) envs

-- |Shortcut to write to the reference to a LTMS's `Node` formatter.
setDebugging ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> Bool -> LTMST s m ()
{-# INLINE setDebugging #-}
setDebugging = setLTMSMutable ltmsDebugging

-- |Shortcut to write to the reference to a LTMS's `Node` formatter.
setCheckingContradictions ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> Bool -> LTMST s m ()
{-# INLINE setCheckingContradictions #-}
setCheckingContradictions = setLTMSMutable ltmsCheckingContradictions

-- |Return the `LTMS`'s current `Node` formatter.
getNodeString ::
  (Monad m, NodeDatum d) =>
    LTMS d i r s m -> LTMST s m (Node d i r s m -> String)
{-# INLINE getNodeString #-}
getNodeString = getLTMSMutable ltmsNodeString

-- |Shortcut to write to the reference to a LTMS's `Node` formatter.
setNodeString ::
  (Monad m, NodeDatum d) =>
    LTMS d i r s m -> (Node d i r s m -> String) -> LTMST s m ()
{-# INLINE setNodeString #-}
setNodeString = setLTMSMutable ltmsNodeString

-- |Shortcut to write to the reference to a LTMS's `Node` formatter.
setComplete ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> Bool -> LTMST s m ()
{-# INLINE setComplete #-}
setComplete = setLTMSMutable ltmsComplete

-- |Shortcut to write to the reference to a LTMS's `Node` formatter.
setDelaySat ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> Bool -> LTMST s m ()
{-# INLINE setDelaySat #-}
setDelaySat = setLTMSMutable ltmsDelaySat

-- |Return the `LTMS`'s current datum formatter.
getDatumString ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m (d -> String)
{-# INLINE getDatumString #-}
getDatumString = getLTMSMutable ltmsDatumString
-- |Shortcut to write to the reference to a LTMS's datum formatter.
setDatumString ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> (d -> String) -> LTMST s m ()
{-# INLINE setDatumString #-}
setDatumString = setLTMSMutable ltmsDatumString

-- |When the data associated with `Node`s are all `String`s, we can
-- direct the `LTMS` to display each datum as itself.
setDatumStringViaString :: Monad m => LTMS String i r s m -> LTMST s m ()
setDatumStringViaString ltms = setDatumString ltms id

-- |When the data associated with `Node`s are of a type of class
-- `Show`, we can direct the `LTMS` to display each datum using the
-- `show` instance.
setDatumStringViaShow ::
  (NodeDatum d, Show d, Monad m) => LTMS d i r s m -> LTMST s m ()
setDatumStringViaShow ltms = setDatumString ltms show

-- |Return the `LTMS`'s current informant formatter.
getInformantString ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m (i -> String)
{-# INLINE getInformantString #-}
getInformantString = getLTMSMutable ltmsInformantString
-- |Shortcut to write to the reference to a LTMS's informant formatter.
setInformantString ::
  (Monad m, NodeDatum d) => LTMS d i r s m -> (i -> String) -> LTMST s m ()
{-# INLINE setInformantString #-}
setInformantString = setLTMSMutable ltmsInformantString

-- |When the informants associated with `JustRule`s are all
-- `String`s, we can direct the `LTMS` to display each informant as
-- itself.
setInformantStringViaString ::
  (Monad m, NodeDatum d) => LTMS d String r s m -> LTMST s m ()
setInformantStringViaString ltms = setInformantString ltms id

-- |When the informants associated with `JustRule`s are of a type of
-- class `Show`, we can direct the `LTMS` to display each datum using
-- the `show` instance.
setInformantStringViaShow ::
  (Show i, Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m ()
setInformantStringViaShow ltms = setInformantString ltms show

-- |Return the `LTMS`'s current rule-queueing procedure.
getEnqueueProcedure ::
  (Monad m, NodeDatum d) =>
    LTMS d i r s m -> LTMST s m (r -> LTMST s m ())
{-# INLINE getEnqueueProcedure #-}
getEnqueueProcedure = getLTMSMutable ltmsEnqueueProcedure
-- |Shortcut to write to the reference to a LTMS's rule-queueing procedure.
setEnqueueProcedure ::
  (Monad m, NodeDatum d) =>
    LTMS d i r s m -> (r -> LTMST s m ()) -> LTMST s m ()
{-# INLINE setEnqueueProcedure #-}
setEnqueueProcedure = setLTMSMutable ltmsEnqueueProcedure

-- |Get the next node counter value, incrementing for future accesses.
nextNodeCounter :: (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m Int
nextNodeCounter jtms = sttLayer $ do
  let nodeCounter = ltmsNodeCounter jtms
  nodeId <- readSTRef nodeCounter
  writeSTRef nodeCounter $ 1 + nodeId
  return nodeId

-- |Get the next clause counter value, incrementing for future accesses.
nextClauseCounter :: (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m Int
nextClauseCounter jtms = sttLayer $ do
  let clauseCounter = ltmsClauseCounter jtms
  clauseId <- readSTRef clauseCounter
  writeSTRef clauseCounter $ 1 + clauseId
  return clauseId

{- ----------------------------------------------------------------- -}

-- |Wrapper for the datum associated with a node of the `ATMS`.
--
-- Translated from @tms-node@ in @ltms.lisp@.
--
-- > (defstruct (tms-node (:PRINT-FUNCTION print-tms-node))
-- >   (index 0)                    ; unique namer for nodes
-- >   (datum nil)                     ; positive inference engine datum.
-- >   (label :UNKNOWN)             ; :UNKNOWN, :TRUE, or :FALSE.
-- >   (support nil)                ; clause which supports it,
-- >   (true-clauses nil)           ; clauses in which this node is true
-- >   (false-clauses nil)          ; clauses in which this node is false
-- >   (mark nil)                   ; marker for sweep algorithms
-- >   (assumption? nil)
-- >   (true-rules nil)             ; rules run when the node is true
-- >   (false-rules nil)            ; rules run when the node is false
-- >   (ltms nil)                   ; LTMS it is part of.
-- >   (true-literal nil)              ; True literal.
-- >   (false-literal nil))         ; False literal.
-- > ;; The last two fields have their names changed because there is
-- > ;; an obscure bug in ACLPC that causes it to barf if a field is
-- > ;; named TRUE or FALSE, even if there is a conc-name to be added.
-- > ;; The new names are more descriptive anyway.
data (Monad m, NodeDatum d) => Node d i r s m = Node

-- | TODO
--
-- Translated from @clause@ in @ltms.lisp@.
--
-- > (defstruct (clause (:PRINT-FUNCTION print-clause))
-- >   (index 0)       ; Unique namer
-- >   (informant nil)
-- >   (literals nil)  ; a list of (<node> . <truth>)
-- >   (pvs 0)         ; Number of terms which potentially violate it.
-- >   (length 0)      ; Number of literals.
-- >   (sats 0)   ; Number of terms which satisfy it.
-- >   (status nil))   ; :SUBSUMED | :QUEUED | :DIRTY | :NOT-INDEXED | nil
newtype (Monad m, NodeDatum d) => Clause d i r s m = Clause ()

$(makeAccessors [t|LTMS|] [t|LTMST|] [|sttLayer|] [t|NodeDatum|] [
     ("Nodes", inList $ withParams [t|Node|], [|ltmsNodes|]),
     ("Clauses", inList $ withParams [t|Clause|], [|ltmsClauses|]),
     ("Debugging", noTyParams [t|Bool|], [|ltmsDebugging|]),
     ("PendingContradictions", inList $ withParams [t|Node|], [|ltmsPendingContradictions|]),
     ("CheckingContradictions", noTyParams [t|Bool|], [|ltmsCheckingContradictions|]),
     ("Complete", noTyParams [t|Bool|], [|ltmsComplete|]),
     ("ViolatedClauses", inList $ withParams [t|Clause|], [|ltmsViolatedClauses|]),
     ("DelaySat", noTyParams [t|Bool|], [|ltmsDelaySat|])
     ]
   [])

-- | TODO
--
-- Translated from @print-ltms@ in @ltms.lisp@.
--
-- > (defun print-ltms (ltms stream ignore)
-- >    (declare (ignore ignore))
-- >    (format stream "#<LTMS: ~A>" (ltms-title ltms)))
printLtms :: a
printLtms = error "TODO"

-- | TODO
--
-- Translated from @print-tms-node@ in @ltms.lisp@.
--
-- > (defun print-tms-node (node stream ignore)
-- >    (declare (ignore ignore))
-- >    (format stream "#<NODE: ~A>" (node-string node)))
printTmsNode :: a
printTmsNode = error "TODO"

-- | TODO
--
-- Translated from @print-clause@ in @ltms.lisp@.
--
-- > (defun print-clause (clause stream ignore)
-- >    (declare (ignore ignore))
-- >    (format stream "#<Clause ~D>" (clause-index clause)))
printClause :: a
printClause = error "TODO"

-- | TODO
--
-- Translated from @node-string@ in @ltms.lisp@.
--
-- > (defun node-string (node)
-- >   (funcall (ltms-node-string (tms-node-ltms node)) node))
nodeString :: a
nodeString = error "TODO"

-- > (defmacro debugging-ltms (ltms msg &optional node &rest args)
-- >   `(when (ltms-debugging ,ltms)
-- >      (format *trace-output*
-- >         ,msg (if ,node (node-string ,node)) ,@args)))

-- | TODO
--
-- Translated from @ltms-error@ in @ltms.lisp@.
--
-- > (defun ltms-error (string &optional thing) (error string thing))
ltmsError :: a
ltmsError = error "TODO"

-- | TODO
--
-- Translated from @default-node-string@ in @ltms.lisp@.
--
-- > (defun default-node-string (n)
-- >   (format nil "~A" (TMSnode.datum n)))
defaultNodeString :: a
defaultNodeString = error "TODO"

-- | TODO
--
-- Translated from @satisfied-clause?@ in @ltms.lisp@.
--
-- > (defmacro satisfied-clause? (clause) `(> (clause-sats ,clause) 0))
isSatisfiedClause :: a
isSatisfiedClause = error "TODO"

-- | TODO
--
-- Translated from @violated-clause?@ in @ltms.lisp@.
--
-- > (defmacro violated-clause? (clause) `(= (clause-pvs ,clause) 0))
isViolatedClause :: a
isViolatedClause = error "TODO"

-- | TODO
--
-- Translated from @walk-clauses@ in @ltms.lisp@.
--
-- > (defmacro walk-clauses (ltms f)
-- >   `(if (ltms-complete ,ltms)
-- >        (walk-trie ,f (ltms-clauses ,ltms))
-- >        (mapc ,f (ltms-clauses ,ltms))))
-- > ;;; Basic inference-engine interface.
walkClauses :: a
walkClauses = error "TODO"

-- | TODO
--
-- Translated from @create-ltms@ in @ltms.lisp@.
--
-- > (defun create-ltms (title &key (node-string 'default-node-string)
-- >                     (debugging NIL)
-- >                     (checking-contradictions T)
-- >                     (contradiction-handler 'ask-user-handler)
-- >                     (enqueue-procedure NIL)
-- >                     (cache-datums? T)
-- >                     (complete nil)
-- >                     (delay-sat T)
-- >                     &aux ltms)
-- >    ;; The CACHE-DATUMS? flag is new in this version.  When used as
-- >    ;; part of a larger system, the internal TMS cache tends to be redundant.
-- >    ;; Creating an LTMS with this flag turned off avoids the storage overhead
-- >    ;; of this redundancy, while still leaving a default mechanism in place
-- >    ;; for experimentation and systems that choose to use it.
-- >   (setq ltms
-- >    (make-ltms :TITLE title
-- >               :NODES (if cache-datums? (make-hash-table :TEST #'equal))
-- >               :NODE-STRING node-string
-- >               :DEBUGGING debugging
-- >               :CHECKING-CONTRADICTIONS checking-contradictions
-- >               :ENQUEUE-PROCEDURE enqueue-procedure
-- >               :CONTRADICTION-HANDLERS (list contradiction-handler)
-- >               :DELAY-SAT delay-sat
-- >               :COMPLETE complete))
-- >   ltms)
createLtms :: a
createLtms = error "TODO"

-- | TODO
--
-- Translated from @change-ltms@ in @ltms.lisp@.
--
-- > (defun change-ltms (ltms &key (contradiction-handler nil contra?)
-- >                          node-string
-- >                          enqueue-procedure
-- >                          (debugging nil debugging?)
-- >                          (checking-contradictions nil checking?)
-- >                          (complete nil complete?)
-- >                          (delay-sat nil delay-sat?))
-- >   (if node-string (setf (ltms-node-string ltms) node-string))
-- >   (if debugging? (setf (ltms-debugging ltms) debugging))
-- >   (if checking? (setf (ltms-checking-contradictions ltms)
-- >                  checking-contradictions))
-- >   (if contra?
-- >       (setf (ltms-contradiction-handlers ltms)
-- >        (list contradiction-handler)))
-- >   (if enqueue-procedure
-- >       (setf (ltms-enqueue-procedure ltms) enqueue-procedure))
-- >   (if complete? (setf (ltms-complete ltms) complete))
-- >   (if delay-sat? (setf (ltms-delay-sat ltms) delay-sat)))
changeLtms :: a
changeLtms = error "TODO"

-- | TODO
--
-- Translated from @unknown-node?@ in @ltms.lisp@.
--
-- > (defun unknown-node? (node) (eq (TMSnode.label node) :UNKNOWN))
isUnknownNode :: a
isUnknownNode = error "TODO"

-- | TODO
--
-- Translated from @known-node?@ in @ltms.lisp@.
--
-- > (defun known-node? (node) (not (eq (TMSnode.label node) :UNKNOWN)))
isKnownNode :: a
isKnownNode = error "TODO"

-- | TODO
--
-- Translated from @true-node?@ in @ltms.lisp@.
--
-- > (defun true-node? (node) (eq (TMSnode.label node) :TRUE))
isTrueNode :: a
isTrueNode = error "TODO"

-- | TODO
--
-- Translated from @false-node?@ in @ltms.lisp@.
--
-- > (defun false-node? (node) (eq (TMSnode.label node) :FALSE))
isFalseNode :: a
isFalseNode = error "TODO"

-- | TODO
--
-- Translated from @tms-create-node@ in @ltms.lisp@.
--
-- > (defun tms-create-node (ltms datum &key assumptionp)
-- >   (if (and (ltms-nodes ltms) (gethash datum (ltms-nodes ltms)))
-- >       (ltms-error "Two nodes with same datum:" datum))
-- >   (let ((node (make-tms-node :INDEX (incf (ltms-node-counter ltms))
-- >                         :DATUM datum
-- >                         :ASSUMPTION? assumptionp
-- >                         :LTMS ltms)))
-- >     (setf (tms-node-true-literal node) (cons node :TRUE))
-- >     (setf (tms-node-false-literal node) (cons node :FALSE))
-- >     (if (ltms-nodes ltms) ;; Insert if locally caching
-- >        (setf (gethash datum (ltms-nodes ltms)) node))
-- >     (when (and (ltms-complete ltms)
-- >           (> (ltms-node-counter ltms) (ltms-cons-size ltms)))
-- >       (setf (ltms-conses ltms) nil)
-- >       (incf (ltms-cons-size ltms) 50.)
-- >       (dotimes (i (ltms-cons-size ltms))
-- >    (push (cons nil nil) (ltms-conses ltms))))
-- >     node))
tmsCreateNode :: a
tmsCreateNode = error "TODO"

-- | TODO
--
-- Translated from @enable-assumption@ in @ltms.lisp@.
--
-- > (defun enable-assumption (node label)
-- >   (cond ((not (TMSnode.isAssumption node))
-- >     (ltms-error "Can't enable the non-assumption ~A" node))
-- >    ((eq (tms-node-label node) label)
-- >     (setf (tms-node-support node) :ENABLED-ASSUMPTION))
-- >    ((eq (tms-node-label node) :UNKNOWN)
-- >     (top-set-truth node label :ENABLED-ASSUMPTION))
-- >    (t (ltms-error "Can't set an already set node" node))))
enableAssumption :: a
enableAssumption = error "TODO"

-- | TODO
--
-- Translated from @convert-to-assumption@ in @ltms.lisp@.
--
-- > (defun convert-to-assumption (node)
-- >   (unless (tms-node-isAssumption node)
-- >     (debugging-ltms (tms-node-ltms node)
-- >                "~%Converting ~A into an assumption" node)
-- >     (setf (tms-node-isAssumption node) T)))
convertToAssumption :: a
convertToAssumption = error "TODO"

-- | TODO
--
-- Translated from @retract-assumption@ in @ltms.lisp@.
--
-- > (defun retract-assumption (node)
-- >   (when (and (known-node? node)
-- >         (eq (tms-node-support node) :ENABLED-ASSUMPTION))
-- >     (find-alternative-support (tms-node-ltms node)
-- >                          (propagate-unknownness node))))
retractAssumption :: a
retractAssumption = error "TODO"

-- > ;;; Adding formulas to the LTMS.

-- | TODO
--
-- Translated from @add-formula@ in @ltms.lisp@.
--
-- > (defun add-formula (ltms formula &optional informant)
-- >   (setq informant (list :IMPLIED-BY formula informant))
-- >   (dolist (clause (normalize ltms formula))
-- >     (unless (eq :TRUE (setq clause (simplify-clause clause)))
-- >    (add-clause-internal clause informant T)))
-- >   (check-for-contradictions ltms))
addFormula :: a
addFormula = error "TODO"

-- | TODO
--
-- Translated from @simplify-clause@ in @ltms.lisp@.
--
-- > (defun simplify-clause (literals)
-- >   (setq literals (sort-clause literals))
-- >   (do ((tail literals next)
-- >        (next (cdr literals) (cdr next)))
-- >       ((null next) literals)
-- >     (cond ((not (eq (caar tail) (caar next))))
-- >      ((not (eq (cdar tail) (cdar next)))
-- >       (return-from simplify-clause :TRUE))
-- >      (t (rplacd tail (cdr next))))))
simplifyClause :: a
simplifyClause = error "TODO"

-- | TODO
--
-- Translated from @sort-clause@ in @ltms.lisp@.
--
-- > (defun sort-clause (literals)
-- >   (sort (copy-list literals) ;; Avoids shared structure bugs.
-- >      #'< :KEY #'(lambda (n) (tms-node-index (car n)))))
sortClause :: a
sortClause = error "TODO"

-- > (defvar *ltms*)

-- | TODO
--
-- Translated from @normalize@ in @ltms.lisp@.
--
-- > (defun normalize (*ltms* exp) (normalize-1 exp nil))
normalize :: a
normalize = error "TODO"

-- | TODO
--
-- Translated from @normalize-1@ in @ltms.lisp@.
--
-- > (defun normalize-1 (exp negate)
-- >   (case (and (listp exp) (car exp))
-- >     (:IMPLIES (if negate
-- >              (nconc (normalize-1 (cadr exp) nil)
-- >                     (normalize-1 (caddr exp) t))
-- >              (disjoin (normalize-1 (cadr exp) t)
-- >                       (normalize-1 (caddr exp) nil))))
-- >     (:IFF (normalize-iff exp negate))
-- >     (:OR (if negate (normalize-conjunction exp t)
-- >         (normalize-disjunction exp nil)))
-- >     (:AND (if negate (normalize-disjunction exp t)
-- >                 (normalize-conjunction exp nil)))
-- >     (:NOT (normalize-1 (cadr exp) (not negate)))
-- >     (:TAXONOMY (normalize-tax exp negate))
-- >     (t (if negate `((,(tms-node-false-literal (find-node *ltms* exp))))
-- >              `((,(tms-node-true-literal (find-node *ltms* exp))))))))
normalize1 :: a
normalize1 = error "TODO"

-- | TODO
--
-- Translated from @normalize-tax@ in @ltms.lisp@.
--
-- > (defun normalize-tax (exp negate)
-- >   (normalize-1 `(:AND (:OR ,@ (copy-list (cdr exp))) ;one must be true
-- >                  ;; The list is copied above to prevent very nasty bugs, since
-- >                  ;; the rest of normalize side effects structure continually for
-- >                  ;; efficiency.
-- >                  ,@ (do ((firsts (cdr exp) (cdr firsts))
-- >                   (rests (cddr exp) (cdr rests))
-- >                   (result nil))
-- >                  ((null rests) result)
-- >                (dolist (other rests)
-- >                  (push `(:NOT (:AND ,(car firsts) ,other))
-- >                        result))))
-- >           negate))
normalizeTax :: a
normalizeTax = error "TODO"

-- | TODO
--
-- Translated from @normalize-conjunction@ in @ltms.lisp@.
--
-- > (defun normalize-conjunction (exp negate)
-- >   (mapcan #'(lambda (sub) (normalize-1 sub negate)) (cdr exp)))
normalizeConjunction :: a
normalizeConjunction = error "TODO"

-- | TODO
--
-- Translated from @normalize-iff@ in @ltms.lisp@.
--
-- > (defun normalize-iff (exp negate)
-- >   (nconc (normalize-1 `(:IMPLIES ,(cadr exp) ,(caddr exp)) negate)
-- >     (normalize-1 `(:IMPLIES ,(caddr exp) ,(cadr exp)) negate)))
normalizeIff :: a
normalizeIff = error "TODO"

-- | TODO
--
-- Translated from @normalize-disjunction@ in @ltms.lisp@.
--
-- > (defun normalize-disjunction (exp negate)
-- >   (unless (cdr exp)
-- >     (return-from normalize-disjunction (list nil)))
-- >   (do ((result (normalize-1 (cadr exp) negate))
-- >        (rest (cddr exp) (cdr rest)))
-- >       ((null rest) result)
-- >     (setq result (disjoin (normalize-1 (car rest) negate) result))))
normalizeDisjunction :: a
normalizeDisjunction = error "TODO"

-- | TODO
--
-- Translated from @disjoin@ in @ltms.lisp@.
--
-- > (defun disjoin (conj1 conj2)
-- >   (unless (or conj1 conj2) (return-from disjoin nil))
-- >   (mapcan #'(lambda (disj1)
-- >        (mapcar #'(lambda (disj2) (append disj1 disj2))
-- >                conj2))
-- >      conj1))
disjoin :: a
disjoin = error "TODO"

-- | TODO
--
-- Translated from @find-node@ in @ltms.lisp@.
--
-- > (defun find-node (ltms name)
-- >   (cond ((typep name 'tms-node) name)
-- >    ((if (ltms-nodes ltms) (gethash name (ltms-nodes ltms))))
-- >    ((tms-create-node ltms name))))
findNode :: a
findNode = error "TODO"

-- | TODO
--
-- Translated from @compile-formula@ in @ltms.lisp@.
--
-- > (defmacro compile-formula (run-tms f &optional informant &aux ltms)
-- >   (setq ltms (create-ltms f))
-- >   (add-formula ltms (expand-formula f))
-- >   (generate-code ltms run-tms (if informant `(:IMPLIED-BY ,f ,informant))))
compileFormula :: a
compileFormula = error "TODO"

-- | TODO
--
-- Translated from @generate-code@ in @ltms.lisp@.
--
-- > (defun generate-code (ltms run-tms informant &aux result bound datum)
-- >   (maphash #'(lambda (ignore symbol)
-- >           (when (or (tms-node-true-clauses symbol)
-- >                     (tms-node-false-clauses symbol))
-- >             (setq datum (tms-node-datum symbol))
-- >             (when (listp datum)
-- >               (setf (tms-node-mark symbol) datum)
-- >               (setf (tms-node-datum symbol)
-- >                     (make-symbol (format nil "~A" (cadr datum))))
-- >               (push symbol bound))))
-- >       (ltms-nodes ltms))
-- >   (walk-clauses ltms
-- >            #'(lambda (clause &aux ps ns)
-- >                (dolist (lit (clause-literals clause))
-- >                  (if (eq (cdr lit) :TRUE)
-- >                      (push (tms-node-datum (car lit)) ps)
-- >                      (push (tms-node-datum (car lit)) ns)))
-- >                (push `(add-clause `(,,@ps) `(,,@ns) ,informant)
-- >                      result)))
-- >   `(let ,(mapcar #'(lambda (s)
-- >                 `(,(tms-node-datum s) (find-node ,run-tms ,(tms-node-mark s))))
-- >             bound)
-- >      ,@result))
-- > (defun expand-formula (x)
-- >   (setq x (macroexpand x))
-- >   (cond ((not (listp x)) x)
-- >    ((case (macroexpand (car x))
-- >       (QUOTE (partial (cadr x)))
-- >       (LIST (mapcar #'expand-formula (cdr x)))
-- >       (LIST* (if (cddr x)
-- >                  (cons (expand-formula (cadr x))
-- >                        (expand-formula `(LIST* .,(cddr x))))
-- >                  (expand-formula (cadr x))))
-- >       (CONS (cons (expand-formula (cadr x))
-- >                   (mapcar #'expand-formula (caddr x))))))
-- >    (t x)))
generateCode :: a
generateCode = error "TODO"

-- | TODO
--
-- Translated from @partial@ in @ltms.lisp@.
--
-- > (defun partial (x)
-- >   (cond ((null x) x)
-- >    ((keywordp x) x)
-- >    ((not (listp x)) `',x)
-- >    (t (cons (partial (car x)) (partial (cdr x))))))
partial :: a
partial = error "TODO"

-- > ;;; Adding clauses

-- | TODO
--
-- Translated from @add-clause@ in @ltms.lisp@.
--
-- > (defun add-clause (true-nodes false-nodes &optional informant)
-- >   (add-clause-internal (nconc (mapcar #'tms-node-true-literal true-nodes)
-- >                          (mapcar #'tms-node-false false-nodes))
-- >                   informant
-- >                   nil))
addClause :: a
addClause = error "TODO"

-- | TODO
--
-- Translated from @add-clause-internal@ in @ltms.lisp@.
--
-- > (defun add-clause-internal (literals informant internal &aux ltms)
-- >   (setq ltms (tms-node-ltms
-- >           (or (caar literals)
-- >               (ltms-error "Total contradiction: Null clause" informant))))
-- >   (if (ltms-complete ltms)
-- >       (full-add-clause ltms literals informant)
-- >       (push (bcp-add-clause ltms literals informant)
-- >        (ltms-clauses ltms)))
-- >   (unless internal (check-for-contradictions ltms)))
addClauseInternal :: a
addClauseInternal = error "TODO"

-- | TODO
--
-- Translated from @bcp-add-clause@ in @ltms.lisp@.
--
-- > (defun bcp-add-clause (ltms literals informant &optional (index T)
-- >                                           &aux cl label)
-- >   (setq cl (make-clause :INDEX (incf (ltms-clause-counter ltms))
-- >                    :LITERALS literals
-- >                    :INFORMANT informant
-- >                    :LENGTH (length literals)))
-- >   (dolist (term literals)
-- >     (if (eq :UNKNOWN (setq label (tms-node-label (car term))))
-- >    (incf (clause-pvs cl)))
-- >     (ecase (cdr term)
-- >       (:TRUE
-- >    (if index (insert-true-clause cl (car term)))
-- >    (when (eq label :TRUE)
-- >      (incf (clause-sats cl)) (incf (clause-pvs cl))))
-- >       (:FALSE
-- >        (if index (insert-false-clause cl (car term)))
-- >        (when (eq label :FALSE)
-- >     (incf (clause-sats cl)) (incf (clause-pvs cl))))))
-- >   (if index (check-clauses ltms (list cl)))
-- >   cl)
bcpAddClause :: a
bcpAddClause = error "TODO"

-- | TODO
--
-- Translated from @insert-true-clause@ in @ltms.lisp@.
--
-- > (defun insert-true-clause (cl node)
-- >   (push cl (tms-node-true-clauses node)))
insertTrueClause :: a
insertTrueClause = error "TODO"

-- | TODO
--
-- Translated from @insert-false-clause@ in @ltms.lisp@.
--
-- > (defun insert-false-clause (cl node)
-- >   (push cl (tms-node-false-clauses node)))
insertFalseClause :: a
insertFalseClause = error "TODO"

-- | TODO
--
-- Translated from @add-nogood@ in @ltms.lisp@.
--
-- > (defun add-nogood (culprit sign assumptions &aux trues falses)
-- >   (dolist (a assumptions (add-clause trues falses 'NOGOOD))
-- >     (ecase (if (eq a culprit) sign (tms-node-label a))
-- >       (:TRUE (push a falses))
-- >       (:FALSE (push a trues)))))
addNogood :: a
addNogood = error "TODO"

-- > ;;; Boolean Constraint Propagation.
-- > (proclaim '(special *clauses-to-check*))

-- | TODO
--
-- Translated from @check-clauses@ in @ltms.lisp@.
--
-- > (defun check-clauses (ltms *clauses-to-check*)
-- >   (debugging-ltms ltms "~% Beginning propagation...")
-- >   (do nil ((null *clauses-to-check*))
-- >     (check-clause ltms (pop *clauses-to-check*))))
checkClauses :: a
checkClauses = error "TODO"

-- | TODO
--
-- Translated from @check-clause@ in @ltms.lisp@.
--
-- > (defun check-clause (ltms clause &aux unknown-pair)
-- >   (cond ((violated-clause? clause)
-- >     (pushnew clause (ltms-violated-clauses ltms)))
-- >    ((= (clause-pvs clause) 1)
-- >     ;; Exactly one term of the clause remains that can
-- >     ;; satisfy the clause, so deduce that term
-- >     (setq unknown-pair (find-unknown-pair clause))
-- >     (when unknown-pair ;must check, because it might have other
-- >       (set-truth (car unknown-pair) ; support
-- >                  (cdr unknown-pair) clause)))))
checkClause :: a
checkClause = error "TODO"

-- | TODO
--
-- Translated from @find-unknown-pair@ in @ltms.lisp@.
--
-- > (defun find-unknown-pair (clause)
-- >   (dolist (term-pair (clause-literals clause))
-- >     (if (unknown-node? (car term-pair)) (return term-pair))))
findUnknownPair :: a
findUnknownPair = error "TODO"

-- | TODO
--
-- Translated from @top-set-truth@ in @ltms.lisp@.
--
-- > (defun top-set-truth (node value reason &aux *clauses-to-check*)
-- >   (set-truth node value reason)
-- >   (check-clauses (tms-node-ltms node) *clauses-to-check*)
-- >   (check-for-contradictions (tms-node-ltms node)))
topSetTruth :: a
topSetTruth = error "TODO"

-- | TODO
--
-- Translated from @set-truth@ in @ltms.lisp@.
--
-- > (defun set-truth (node value reason &aux ltms enqueuef)
-- >   (setq ltms (tms-node-ltms node)
-- >    enqueuef (ltms-enqueue-procedure ltms))
-- >   (debugging-ltms
-- >     ltms "~%  Setting ~A to ~A, via ~A." node value reason)
-- >   (setf (tms-node-support node) reason)
-- >   (setf (tms-node-label node) value)
-- >   (ecase value ;figure out which set of rules to queue up
-- >     (:TRUE (when enqueuef
-- >         (dolist (rule (tms-node-true-rules node))
-- >           (funcall enqueuef rule))
-- >         (setf (tms-node-true-rules node) nil))
-- >       (dolist (clause (tms-node-true-clauses node))
-- >         (incf (clause-sats clause)))
-- >       (dolist (clause (tms-node-false-clauses node))
-- >         (if (< (decf (clause-pvs clause)) 2)
-- >             (push clause *clauses-to-check*))))
-- >     (:FALSE (when enqueuef
-- >          (dolist (rule (tms-node-false-rules node))
-- >            (funcall enqueuef rule)))
-- >        (setf (tms-node-false-rules node) nil)
-- >       (dolist (clause (tms-node-false-clauses node))
-- >         (incf (clause-sats clause)))
-- >        (dolist (clause (tms-node-true-clauses node))
-- >          (if (< (decf (clause-pvs clause)) 2)
-- >              (push clause *clauses-to-check*))))))
setTruth :: a
setTruth = error "TODO"

-- > ;;; Retracting an assumption.

-- | TODO
--
-- Translated from @propagate-unknownness@ in @ltms.lisp@.
--
-- > (defun propagate-unknownness (in-node)
-- >   (let (node old-value node2 unknown-queue ltms)
-- >     (setq ltms (tms-node-ltms in-node))
-- >     (do ((forget-queue (cons in-node nil) (nconc forget-queue new))
-- >     (new nil nil))
-- >    ((null forget-queue) unknown-queue)
-- >       (setq forget-queue (prog1 (cdr forget-queue)
-- >                            (rplacd forget-queue unknown-queue)
-- >                            (setq unknown-queue forget-queue))
-- >        node (car unknown-queue))
-- >       (debugging-ltms ltms "~% Retracting ~A." node)
-- >       (setq old-value (tms-node-label node))
-- >       (setf (tms-node-label node) :UNKNOWN)
-- >       (setf (tms-node-support node) nil)
-- >       (dolist (clause (ecase old-value
-- >                    (:TRUE (tms-node-false-clauses node))
-- >                    (:FALSE (tms-node-true-clauses node))))
-- >    (when (= (incf (clause-pvs clause)) 2)
-- >      (when (setq node2 (clause-consequent clause))
-- >        (push node2 new))))
-- >       (if (ltms-complete ltms)
-- >      (propagate-more-unknownness old-value node ltms)))))
propagateUnknownness :: a
propagateUnknownness = error "TODO"

-- | TODO
--
-- Translated from @clause-consequent@ in @ltms.lisp@.
--
-- > (defun clause-consequent (clause)
-- >   (dolist (term-pair (clause-literals clause))
-- >     (when (eq (tms-node-label (car term-pair)) (cdr term-pair))
-- >       (return (if (eq clause (tms-node-support (car term-pair)))
-- >              (car term-pair))))))
clauseConsequent :: a
clauseConsequent = error "TODO"

-- | TODO
--
-- Translated from @find-alternative-support@ in @ltms.lisp@.
--
-- > (defun find-alternative-support (ltms nodes)
-- >   (dolist (node nodes)
-- >     (when (unknown-node? node)
-- >       (check-clauses ltms (tms-node-true-clauses node))
-- >       (check-clauses ltms (tms-node-false-clauses node))))
-- >   (if (eq T (ltms-complete ltms)) (ipia ltms)))
findAlternativeSupport :: a
findAlternativeSupport = error "TODO"

-- > ;;; Contradiction handling interface.

-- | TODO
--
-- Translated from @check-for-contradictions@ in @ltms.lisp@.
--
-- > (defun check-for-contradictions (ltms &aux violated-clauses)
-- >   (setq violated-clauses
-- >    (delete-if-not #'(lambda (c) (violated-clause? c))
-- >                   (ltms-violated-clauses ltms)))
-- >   (setf (ltms-violated-clauses ltms) violated-clauses) ;; Cache them.
-- >   (if violated-clauses (contradiction-handler ltms violated-clauses)))
checkForContradictions :: a
checkForContradictions = error "TODO"

-- | TODO
--
-- Translated from @contradiction-handler@ in @ltms.lisp@.
--
-- > (defun contradiction-handler (ltms violated-clauses)
-- >   (cond ((not (ltms-checking-contradictions ltms))
-- >          ;; Update cache of violated clauses
-- >          (setf (ltms-pending-contradictions ltms)
-- >                (delete-if-not #'(lambda (c) (violated-clause? c))
-- >                   (ltms-pending-contradictions ltms)))
-- >          (dolist (vc violated-clauses)
-- >             (when (violated-clause? vc)
-- >                (pushnew vc (ltms-pending-contradictions ltms)))))
-- >    (t (dolist (handler (ltms-contradiction-handlers ltms))
-- >         (if (funcall handler violated-clauses ltms) (return T))))))
contradictionHandler :: a
contradictionHandler = error "TODO"

-- | TODO
--
-- Translated from @without-contradiction-check@ in @ltms.lisp@.
--
-- > (defmacro without-contradiction-check (ltms &body body)
-- >   (contradiction-check ltms nil body))
withoutContradictionCheck :: a
withoutContradictionCheck = error "TODO"

-- | TODO
--
-- Translated from @with-contradiction-check@ in @ltms.lisp@.
--
-- > (defmacro with-contradiction-check (ltms &body body)
-- >   (contradiction-check ltms t body))
withContradictionCheck :: a
withContradictionCheck = error "TODO"

-- | TODO
--
-- Translated from @contradiction-check@ in @ltms.lisp@.
--
-- > (defun contradiction-check (ltms flag body)
-- >   `(let* ((.ltms. ,ltms)
-- >      (.old-value. (ltms-checking-contradictions .ltms.)))
-- >      (unwind-protect
-- >     (progn (setf (ltms-checking-contradictions .ltms.) ,flag)
-- >            ,@body)
-- >        (setf (ltms-checking-contradictions .ltms.) .old-value.))))
contradictionCheck :: a
contradictionCheck = error "TODO"

-- | TODO
--
-- Translated from @with-contradiction-handler@ in @ltms.lisp@.
--
-- > (defmacro with-contradiction-handler (ltms handler &body body)
-- >   `(let ((.ltms. ,ltms))
-- >      (unwind-protect
-- >     (progn (push ,handler (ltms-contradiction-handlers .ltms.))
-- >            ,@ body)
-- >        (pop (ltms-contradiction-handlers .ltms.)))))
withContradictionHandler :: a
withContradictionHandler = error "TODO"

-- | TODO
--
-- Translated from @with-assumptions@ in @ltms.lisp@.
--
-- > (defmacro with-assumptions (assumption-values &body body)
-- >   ;; Allows assumptions to be made safely, and retracted properly
-- >   ;; even if non-local exits occur.
-- >   `(unwind-protect (progn (dolist (av ,assumption-values)
-- >                        (enable-assumption (car av) (cdr av)))
-- >                     ,@ body)
-- >      (dolist (av ,assumption-values) (retract-assumption (car av)))))
withAssumptions :: a
withAssumptions = error "TODO"

-- > ;;; Inquiring about well-founded support

-- | TODO
--
-- Translated from @support-for-node@ in @ltms.lisp@.
--
-- > (defun support-for-node (node &aux result support)
-- >   (cond ((null (setq support (tms-node-support node))) nil)
-- >    ((eq support :ENABLED-ASSUMPTION) :ENABLED-ASSUMPTION)
-- >    (t (dolist (pair (clause-literals support))
-- >         (unless (eq (car pair) node)
-- >           (push (car pair) result)))
-- >       (values result (clause-informant support)))))
supportForNode :: a
supportForNode = error "TODO"

-- | TODO
--
-- Translated from @assumptions-of-node@ in @ltms.lisp@.
--
-- > (defun assumptions-of-node (node)
-- >   (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node)) (list node))
-- >    ((known-node? node)
-- >     (assumptions-of-clause (tms-node-support node)))))
assumptionsOfNode :: a
assumptionsOfNode = error "TODO"

-- | TODO
--
-- Translated from @assumptions-of-clause@ in @ltms.lisp@.
--
-- > (defun assumptions-of-clause (in-clause &aux)
-- >   (do ((clause-queue (list in-clause)
-- >                 (nconc (cdr clause-queue) new-clauses))
-- >        (mark (list nil))
-- >        (node nil)
-- >        (new-clauses nil nil)
-- >        (assumptions nil))
-- >       ((null clause-queue) assumptions)
-- >     (dolist (term-pair (clause-literals (car clause-queue)))
-- >       (setq node (car term-pair))
-- >       (unless (eq (tms-node-mark node) mark)
-- >    (unless (eq (tms-node-label node) (cdr term-pair))
-- >      (cond ((eq :ENABLED-ASSUMPTION (tms-node-support node))
-- >             (push node assumptions))
-- >            ((null (tms-node-support node)) (ltms-error "Node is unknown" node))
-- >            (t (push (tms-node-support node) new-clauses))))
-- >    (setf (tms-node-mark node) mark)))))
assumptionsOfClause :: a
assumptionsOfClause = error "TODO"

-- > ;;; Simple user interface
-- > (proclaim '(special *contra-assumptions*))

-- | TODO
--
-- Translated from @ask-user-handler@ in @ltms.lisp@.
--
-- > (defun ask-user-handler (contradictions ltms)
-- >   (declare (ignore ltms))
-- >   (dolist (contradiction contradictions)
-- >     (if (violated-clause? contradiction)
-- >    (handle-one-contradiction contradiction))))
askUserHandler :: a
askUserHandler = error "TODO"

-- | TODO
--
-- Translated from @handle-one-contradiction@ in @ltms.lisp@.
--
-- > (defun handle-one-contradiction (violated-clause)
-- >    (let ((*contra-assumptions* (assumptions-of-clause violated-clause))
-- >          (the-answer nil))
-- >       (unless *contra-assumptions* (ltms-error "Global contradiction"
-- >                                     violated-clause))
-- >       (format t "~%Contradiction found:")
-- >       (print-contra-list *contra-assumptions*)
-- >       (format t "~%Call (TMS-ANSWER <number>) to retract assumption.")
-- >       (setq the-answer
-- >          (catch 'tms-contradiction-handler
-- >             (cerror "Continue LTRE processing (after retracting an assumption)"
-- >                "LTMS contradiction break")))
-- >       (if the-answer
-- >          (retract-assumption (nth (1- the-answer)
-- >                                 *contra-assumptions*)))))
handleOneContradiction :: a
handleOneContradiction = error "TODO"

-- | TODO
--
-- Translated from @print-contra-list@ in @ltms.lisp@.
--
-- > (defun print-contra-list (nodes)
-- >   (do ((counter 1 (1+ counter))
-- >        (nn nodes (cdr nn)))
-- >       ((null nn))
-- >     (format t "~%~A ~A" counter
-- >        (node-string (car nn)))))
printContraList :: a
printContraList = error "TODO"

-- | TODO
--
-- Translated from @tms-answer@ in @ltms.lisp@.
--
-- > (defun tms-answer (num)
-- >   (if (integerp num)
-- >       (if (> num 0)
-- >      (if (not (> num (length *contra-assumptions*)))
-- >          (throw 'tms-contradiction-handler num)
-- >          (format t "~%Ignoring answer, too big."))
-- >      (format t "~%Ignoring answer, too small"))
-- >       (format t "~%Ignoring answer, must be an integer.")))
tmsAnswer :: a
tmsAnswer = error "TODO"

-- | TODO
--
-- Translated from @avoid-all@ in @ltms.lisp@.
--
-- > (defun avoid-all (contradictions ignore &aux culprits culprit sign)
-- >   (dolist (contradiction contradictions)
-- >     (when (violated-clause? contradiction)
-- >       (unless (setq culprits (assumptions-of-clause contradiction))
-- >    (ltms-error "Total contradiction" contradiction))
-- >       (setq culprit (car culprits)
-- >        sign (tms-node-label culprit))
-- >       (retract-assumption culprit)
-- >       (add-nogood culprit sign culprits)
-- >       t)))
avoidAll :: a
avoidAll = error "TODO"

-- | TODO
--
-- Translated from @clause-antecedents@ in @ltms.lisp@.
--
-- > (defun clause-antecedents (clause &aux result)
-- >   (dolist (pair (clause-literals clause) result)
-- >     (unless (eq (tms-node-support (car pair)) clause)
-- >       (push (car pair) result))))
clauseAntecedents :: a
clauseAntecedents = error "TODO"

-- | TODO
--
-- Translated from @signed-node-string@ in @ltms.lisp@.
--
-- > (defun signed-node-string (node)
-- >   (if (true-node? node) (node-string node)
-- >       (format nil "~:[Unknown~;Not~][~A]"
-- >          (false-node? node) (node-string node))))
signedNodeString :: a
signedNodeString = error "TODO"

-- | TODO
--
-- Translated from @node-consequences@ in @ltms.lisp@.
--
-- > (defun node-consequences (node &aux conseq conseqs)
-- >   (dolist (cl (ecase (tms-node-label node)
-- >            (:TRUE (tms-node-false-clauses node))
-- >            (:FALSE (tms-node-true-clauses node))))
-- >     (unless (eq cl (tms-node-support node))
-- >       (setq conseq (clause-consequent cl))
-- >       (if conseq (push conseq conseqs))))
-- >   conseqs)
nodeConsequences :: a
nodeConsequences = error "TODO"

-- | TODO
--
-- Translated from @why-node@ in @ltms.lisp@.
--
-- > (defun why-node (node)
-- >   (cond ((unknown-node? node)
-- >     (format t "~%~A is unknown." (node-string node))
-- >     nil)
-- >    ((eq :ENABLED-ASSUMPTION (tms-node-support node))
-- >     (format t "~%~A is ~A <~A>"
-- >             (node-string node)
-- >             (tms-node-label node) (tms-node-support node))
-- >     nil)
-- >    (t (format t "~%~A is ~A via ~A on"
-- >               (node-string node)
-- >               (tms-node-label node)
-- >               (or (clause-informant (tms-node-support node))
-- >                   (tms-node-support node)))
-- >       (dolist (term-pair (clause-literals (tms-node-support node)))
-- >         (unless (equal (tms-node-label (car term-pair))
-- >                        (cdr term-pair))
-- >           (format t "~%   ~A is ~A"
-- >                   (node-string (car term-pair))
-- >                   (tms-node-label (car term-pair)))))))
-- >   node)
whyNode :: a
whyNode = error "TODO"

-- | TODO
--
-- Translated from @why-nodes@ in @ltms.lisp@.
--
-- > (defun why-nodes (ltms)
-- >   (maphash #'(lambda (ignore n) (why-node n)) (ltms-nodes ltms)))
whyNodes :: a
whyNodes = error "TODO"

-- > (defvar *line-count*)

-- | TODO
--
-- Translated from @explain-node@ in @ltms.lisp@.
--
-- > (defun explain-node (node &aux *line-count*)
-- >   (unless (eq (tms-node-label node) :UNKNOWN)
-- >     (setq *line-count* 0)
-- >     (maphash #'(lambda (ignore node) (setf (tms-node-mark node) nil))
-- >         (ltms-nodes (tms-node-ltms node)))
-- >     (explain-1 node)))
explainNode :: a
explainNode = error "TODO"

-- | TODO
--
-- Translated from @explain-1@ in @ltms.lisp@.
--
-- > (defun explain-1 (node &aux antecedents)
-- >   (cond ((tms-node-mark node))
-- >    ((eq :ENABLED-ASSUMPTION (tms-node-support node))
-- >     (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~>~15<()~>   Assumption"
-- >             (incf *line-count*) (true-node? node) (node-string node))
-- >     (setf (tms-node-mark node) *line-count*))
-- >    (t (setq antecedents
-- >             (mapcar #'explain-1 (clause-antecedents (tms-node-support node))))
-- >       (format T "~%~3D ~15<~:[(:NOT ~A)~;~A~]~> ~15<~A~>  "
-- >               (incf *line-count*) (true-node? node)
-- >               (node-string node) antecedents)
-- >       (pretty-print-clause (tms-node-support node))
-- >       (setf (tms-node-mark node) *line-count*))))
explain1 :: a
explain1 = error "TODO"

-- | TODO
--
-- Translated from @pretty-print-clauses@ in @ltms.lisp@.
--
-- > (defun pretty-print-clauses (ltms)
-- >   (walk-clauses ltms #'(lambda (l)
-- >                     (format T "~% ")
-- >                     (pretty-print-clause l))))
prettyPrintClauses :: a
prettyPrintClauses = error "TODO"

-- | TODO
--
-- Translated from @pretty-print-clause@ in @ltms.lisp@.
--
-- > (defun pretty-print-clause (clause)
-- >   (format T "(:OR")
-- >   (dolist (literal (clause-literals clause))
-- >     (format T " ~:[(:NOT ~A)~;~A~]"
-- >        (eq :TRUE (cdr literal)) (node-string (car literal))))
-- >   (format T ")"))
prettyPrintClause :: a
prettyPrintClause = error "TODO"

-- | TODO
--
-- Translated from @show-node-consequences@ in @ltms.lisp@.
--
-- > (defun show-node-consequences (node)
-- >   (let ((conseqs (node-consequences node)))
-- >     (cond (conseqs
-- >       (format t "~% Consequences of ~A:" (signed-node-string node))
-- >       (dolist (conseq conseqs)
-- >               (format t "~%  ~A" (signed-node-string conseq))))
-- >      (t (format t "~% ~A has no consequences." (node-string node))))))
showNodeConsequences :: a
showNodeConsequences = error "TODO"

-- | TODO
--
-- Translated from @node-show-clauses@ in @ltms.lisp@.
--
-- > (defun node-show-clauses (node)
-- >   (format t "For ~A:" (node-string node))
-- >   (dolist (cl (tms-node-true-clauses node))
-- >     (format T "~%") (pretty-print-clause cl))
-- >   (dolist (cl (tms-node-false-clauses node))
-- >     (format T "~%") (pretty-print-clause cl)))
nodeShowClauses :: a
nodeShowClauses = error "TODO"

-- | TODO
--
-- Translated from @explore-network@ in @ltms.lisp@.
--
-- > (defun explore-network (node)
-- >   (unless (known-node? node)
-- >      (format t "~% Sorry, ~A not believed." (node-string node))
-- >      (return-from explore-network node))
-- >   (do ((stack nil)
-- >        (current node)
-- >        (mode :ante)
-- >        (options nil)
-- >        (olen 0)
-- >        (done? nil))
-- >       (done? current)
-- >       (cond ((eq mode :ante)
-- >         (why-node current)
-- >         (setq options (if (typep (tms-node-support current) 'clause)
-- >                           (clause-antecedents (tms-node-support current))
-- >                         nil)))
-- >        (t ;; Looking at consequences
-- >         (show-node-consequences current)
-- >         (setq options (node-consequences current))))
-- >       (setq olen (length options))
-- >       (do ((good? nil)
-- >       (choice 0))
-- >      (good? (case good?
-- >                   (q (return-from explore-network current))
-- >                   (c (setq mode :conseq))
-- >                   (a (setq mode :ante))
-- >                   (0 (if stack
-- >                          (setq current (pop stack))
-- >                          (return-from explore-network current)))
-- >                   (t (push current stack)
-- >                      (setq current (nth (1- good?) options)))))
-- >      (format t "~%>>>")
-- >      (setq choice (read))
-- >      (if (or (eq choice 'q)
-- >              (eq choice 'c)
-- >              (eq choice 'a)
-- >              (and (integerp choice)
-- >                   (not (> choice olen))
-- >                   (not (< choice 0))))
-- >          (setq good? choice)
-- >          (format t "~% Must be q, a, c or an integer from 0 to ~D."
-- >                    olen)))))
exploreNetwork :: a
exploreNetwork = error "TODO"
