{-|
Module      : ATMS
Description : Mutable assumption-based truth maintenance systems (ATMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's assumption-based truth
maintenance systems (ATMSes) from Common Lisp to Haskell.

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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{- LANGUAGE ScopedTypeVariables #-}

module Data.TMS.ATMS.ATMST (
  -- * The ATMST monad
  ATMST,
  AtmsErr(CannotRemoveNodeWIthConsequences, InternalNoEmptyEnv, FromMonadFail),
  runATMST,
  setInitialEnvTableAlloc, setEnvTableIncr,
  getInitialEnvTableAlloc, getEnvTableIncr,

  -- * ATMS data structures

  -- ** Component classes
  NodeDatum, contradictionNodeDatum,

  -- ** Top-level ATMS
  ATMS, createATMS, atmsTitle,

  -- *** ATMS components
  getNodes, getJusts, getContradictions, getAssumptions,
  getContradictionNode, getEmptyEnvironment, getNodeString, getJustString,
  getDatumString, getInformantString, getEnqueueProcedure,

  setDatumStringViaString, setDatumStringViaShow,
  setInformantStringViaString, setInformantStringViaShow,

  -- ** Nodes
  Node, nodeDatum, createNode,
  -- *** Node components
  nodeATMS, nodeString, defaultNodeString, getNodeLabel, getNodeRules,
  getNodeConsequences,
  -- *** Setting node status
  assumeNode, makeContradiction, removeNode,

  -- ** Justifications
  JustRule(JustRule), justInformant, justConsequence, justAntecedents,
  Justification, Explanation, justifyNode,

  -- ** Environments and tables
  Env, EnvTable, envIndex, envAssumptions, getEnvNodes,

  -- * Deduction and search utilities
  interpretations,

  -- ** Related to a node
  isTrueNode, isInNode, isInNodeByEnv, isOutNode, isNodeConsistentWith,
  getNodeIsAssumption, getNodeIsContradictory, explainNode,

  -- ** Related to environments
  envIsNogood,

  -- * Printing and debugging

  -- |Functions prefixed @format@ build a computation returning a
  -- `String`.  Functions prefixed @debug@ or @print@ build a unit
  -- computation printing the artifact in question to standard output;
  -- those with prefix @debug@ are generally more verbose.
  debugAtms, printAtms, debugAtmsEnvs,
  printAtmsStatistics,

  -- ** Nodes and node lists
  formatNode, formatNodes, debugNode, printNode,
  whyNodes, whyNode,

  -- ** Environments, labels, and tables
  debugEnv, debugEnvTable, formatNodeLabel,
  debugNogoods,
  printEnv, printNogoods, printEnvs, printEnvTable, printTable,

  -- ** Justifications
  debugJust, printJust, formatJustification

  ) where

import Control.Monad.State
import Control.Monad.ST.Trans
-- import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.Extra
import Data.List
import Data.Symbol
import Data.TMS.Helpers
import Data.TMS.MList
import Data.TMS.Dbg


-- * The @ATMST@ monad transformer
--
-- Construction and manipulation of a ATMS happens inside this monad
-- wrapper.

-- |Errors which can arise from ATMS operations.
data AtmsErr = CannotRemoveNodeWIthConsequences String Int
               -- ^ It is not possible to remove a `Node` from an
               -- `ATMS` after a `JustRule` which uses that `Node` is
               -- added to the `ATMS`.
             | InternalNoEmptyEnv
               -- ^ Internal error called when there is no internal
               -- default empty `Env` associated with this `ATMS`.
               -- Should never be signaled for an `ATMS` created with
               -- `createATMS`, since this latter function does set up
               -- the default empty environment before returning the
               -- new `ATMS`.
             | InternalNoContraNode
               -- ^ Internal error called when there is no internal
               -- default contradictory `Node` associated with this
               -- `ATMS`.  Should never be signaled for an `ATMS`
               -- created with `createATMS`, since this latter
               -- function does set up the default contradiction node
               -- before returning the new `ATMS`.
             | FromMonadFail String
               -- ^ Indicates a pattern-matching failure within an
               -- `ATMST` operation.
  deriving Show

{- ===== Internal state of an ATMST. =================================== -}

-- |Internal state of an ATMST process
data AtmstState = AtmstState {
  initialEnvTableAlloc :: Int,
  envTableIncr :: Int
  }

-- |Initial state of an ATMST process.
initialAtmstState :: AtmstState
initialAtmstState = AtmstState 50 75

-- |Update the initial table size of an ATMST state.
withInitialEnvTableAlloc :: AtmstState -> Int -> AtmstState
withInitialEnvTableAlloc (AtmstState _ ei) ia = AtmstState ia ei

-- |Update the table increment size of an ATMST state.
withEnvTableIncr :: AtmstState -> Int -> AtmstState
withEnvTableIncr (AtmstState ia _) ei = AtmstState ia ei

{- ===== ATMST definition. ============================================= -}

-- |The process of building and using a mutable ATMS.
type ATMSTInner s m a =
  Monad m => ExceptT AtmsErr (StateT AtmstState (STT s m)) a

-- |The process of building and using a mutable ATMS.
newtype Monad m => ATMST s m a = AtmsT { unwrap :: ATMSTInner s m a }

-- |Internal unwrapper preserving rank-2 polymorphism of the state
-- thread in the wrapper `STT`.
unwrap2 :: Monad m => (forall s . ATMST s m a) -> (forall s . ATMSTInner s m a)
unwrap2 (AtmsT m) = m

instance (Monad m) => Functor (ATMST s m) where
  fmap f (AtmsT m) = AtmsT $ do
    v <- m
    return $ f v

instance (Monad m, Functor m) => Applicative (ATMST s m) where
  pure v = AtmsT $ pure v
  (AtmsT m1) <*> (AtmsT m2) = AtmsT $ do
    f <- m1
    v <- m2
    return (f v)

instance (Monad m, Functor m) => Monad (ATMST s m) where
  -- (>>=) :: ATMST s m a -> (a -> ATMST s m b) -> ATMST s m b
  (AtmsT m) >>= f = AtmsT $ m >>= (unwrap . f)

  -- (>>) :: ATMST s m a -> ATMST s m b -> ATMST s m b
  (AtmsT m1) >> (AtmsT m2) = AtmsT $ m1 >> m2

  -- return :: a -> ATMST s m a
  return v = AtmsT $ return v

instance MonadTrans (ATMST s) where
  lift m = AtmsT $ lift $ lift $ lift m

instance MonadIO m => MonadIO (ATMST s m) where
  liftIO = lift . liftIO

-- |Lift `STT` behavior to the `ATMST` level.
sttLayer :: Monad m => STT s m r -> ATMST s m r
sttLayer md = AtmsT $ lift $ lift $ md

-- |Lift `ExceptT` behavior to the `ATMST` level.
exceptLayer ::
  Monad m => ExceptT AtmsErr (StateT AtmstState (STT s m)) r -> ATMST s m r
exceptLayer = AtmsT

-- |Lift `StateT` behavior to the `ATMST` level.
stateLayer ::
  Monad m => StateT AtmstState (STT s m) r -> ATMST s m r
stateLayer = AtmsT . lift

instance Monad m => MonadFail (ATMST s m) where
  fail s = exceptLayer $ throwE $ FromMonadFail s

-- |Retrieve the current initial `Env` table size setting.
getInitialEnvTableAlloc :: Monad m => ATMST s m Int
getInitialEnvTableAlloc = stateLayer $ fmap initialEnvTableAlloc get

-- |Retrieve the current initial `Env` table size setting.
setInitialEnvTableAlloc :: Monad m => Int -> ATMST s m ()
setInitialEnvTableAlloc ia = stateLayer $ modify (`withInitialEnvTableAlloc` ia)

-- |Retrieve the current initial `Env` table size setting.
getEnvTableIncr :: Monad m => ATMST s m Int
getEnvTableIncr = stateLayer $ fmap envTableIncr get

-- |Retrieve the current initial `Env` table size setting.
setEnvTableIncr :: Monad m => Int -> ATMST s m ()
setEnvTableIncr ia = stateLayer $ modify (`withEnvTableIncr` ia)

-- |Execute a computation in the `ATMST` monad transformer.
runATMST :: Monad m => (forall s . ATMST s m r) -> m (Either AtmsErr r)
runATMST atmst = do
  let core = unwrap2 atmst
      afterExcept = runExceptT core
      afterState = do
        (result, endState) <- runStateT afterExcept initialAtmstState
        return result
  runSTT afterState

{- ----------------------------------------------------------------- -}

-- |Class of type which can be used as the datum underlying `Node`s in
-- an `ATMS`.
class NodeDatum d where
  -- |The datum associated with the contradiction node in a
  -- newly-initialized `ATMS` with `Node` data of this type.
  contradictionNodeDatum :: d

instance NodeDatum String where
  contradictionNodeDatum = "The contradiction"
instance NodeDatum Symbol where
  contradictionNodeDatum = intern "The contradiction"

-- |Top-level representation of an assumption-based truth maintenance
-- system.
data (Monad m, NodeDatum d) => ATMS d i r s m = ATMS {
  -- |Name of this ATMS.
  atmsTitle :: String,
  -- |Unique namer for nodes.
  atmsNodeCounter :: STRef s Int,
  -- |Unique namer for justifications.
  atmsJustCounter :: STRef s Int,
  -- |Unique namer for environments.
  atmsEnvCounter :: STRef s Int,
  -- |Current size of environment table.
  atmsEnvTableAlloc :: STRef s Int,
  -- |List of all TMS nodes.
  atmsNodes :: STRef s [Node d i r s m],
  -- |List of all justifications.
  atmsJusts :: STRef s [JustRule d i r s m],
  -- |List of all contradiction nodes.
  atmsContradictions :: STRef s [Node d i r s m],
  -- |List of all assumption nodes.
  atmsAssumptions :: STRef s [Node d i r s m],
  -- |The environment table.
  atmsEnvTable :: STRef s (EnvTable d i r s m),
  -- |The table of nogood environments.
  atmsNogoodTable :: STRef s (EnvTable d i r s m),
  -- |Canonical empty Env for this ATMS.  This value is not set more
  -- than once, but it created (by `createATMS`) after the ATMS is
  -- allocated, so we use a reference to be able to set it up later.
  atmsEmptyEnv :: STRef s (Maybe (Env d i r s m)),
  -- |Canonical contradiction `Node` for this ATMS.  This value is not
  -- set more than once, but it written (by `createATMS`) after the
  -- ATMS is allocated, so we use a reference to be able to set it up
  -- later.
  atmsContraNode :: STRef s (Maybe (Node d i r s m)),
  -- |Function for formatting a `Node` of this ATMS.
  atmsNodeString :: STRef s (Node d i r s m -> String),
  -- |Function for representing a justification rule.
  atmsJustString :: STRef s (JustRule d i r s m -> String),
  -- |Function for representing the data associated with `Node`s.
  atmsDatumString :: STRef s (d -> String),
  -- |Function for representing the informants of justifications.
  atmsInformantString :: STRef s (i -> String),
  -- |List of external procedures to be executed for this ATMS.
  atmsEnqueueProcedure :: STRef s (r -> ATMST s m ()),
  -- |Set to `True` when we wish to debug this ATMS.
  atmsDebugging :: STRef s Bool
}

-- |Shortcut maker for reading from an `ATMS` reference.
getATMSMutable ::
  (Monad m, NodeDatum d) => (ATMS d i r s m -> STRef s a) -> ATMS d i r s m  -> ATMST s m a
{-# INLINE getATMSMutable #-}
getATMSMutable refGetter atms = sttLayer $ readSTRef (refGetter atms)
-- |Shortcut to write to an ATMS reference.
setATMSMutable ::
  (Monad m, NodeDatum d) =>
    (ATMS d i r s m -> STRef s a) -> ATMS d i r s m -> a -> ATMST s m ()
{-# INLINE setATMSMutable #-}
setATMSMutable refGetter atms envs = sttLayer $ writeSTRef (refGetter atms) envs

-- |Return the `ATMS`'s current `Node` list.
getNodes ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m [Node d i r s m]
{-# INLINE getNodes #-}
getNodes = getATMSMutable atmsNodes

-- |Return the `ATMS`'s current `EnvTable`.
getEnvTable ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (EnvTable d i r s m)
{-# INLINE getEnvTable #-}
getEnvTable = getATMSMutable atmsEnvTable

-- |Return the `ATMS`'s current `EnvTable` for nogood `Env`s.
getNogoodTable ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (EnvTable d i r s m)
{-# INLINE getNogoodTable #-}
getNogoodTable = getATMSMutable atmsNogoodTable

-- |Return the `ATMS`'s current `JustRule` list.
getJusts ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m [JustRule d i r s m]
{-# INLINE getJusts #-}
getJusts = getATMSMutable atmsJusts

-- |Return the `ATMS`'s current contradictions list.
getContradictions ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m [Node d i r s m]
{-# INLINE getContradictions #-}
getContradictions = getATMSMutable atmsContradictions

-- |Return the `ATMS`'s current assumptions list.
getAssumptions ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m [Node d i r s m]
{-# INLINE getAssumptions #-}
getAssumptions = getATMSMutable atmsAssumptions

-- |Return the `ATMS`'s built-in empty environment.
getEmptyEnvironment ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (Env d i r s m)
{-# INLINE getEmptyEnvironment #-}
getEmptyEnvironment atms = do
  maybeEnv <- getATMSMutable atmsEmptyEnv atms
  case maybeEnv of
    Just env -> return env
    Nothing -> exceptLayer $ throwE InternalNoEmptyEnv

-- |Return the `ATMS`'s built-in contradiction node.
getContradictionNode ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (Node d i r s m)
{-# INLINE getContradictionNode #-}
getContradictionNode atms = do
  maybeNode <- getATMSMutable atmsContraNode atms
  case maybeNode of
    Just node -> return node
    Nothing -> exceptLayer $ throwE InternalNoContraNode

-- |Return the `ATMS`'s current `Node` formatter.
getNodeString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (Node d i r s m -> String)
{-# INLINE getNodeString #-}
getNodeString = getATMSMutable atmsNodeString
-- |Shortcut to write to the reference to a ATMS's `Node` formatter.
setNodeString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> (Node d i r s m -> String) -> ATMST s m ()
{-# INLINE setNodeString #-}
setNodeString = setATMSMutable atmsNodeString

-- |Return the `ATMS`'s current `JustRule` formatter.
getJustString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (JustRule d i r s m -> String)
{-# INLINE getJustString #-}
getJustString = getATMSMutable atmsJustString
-- |Shortcut to write to the reference to a ATMS's `JustRule` formatter.
setJustString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> (JustRule d i r s m -> String) -> ATMST s m ()
{-# INLINE setJustString #-}
setJustString = setATMSMutable atmsJustString

-- |Return the `ATMS`'s current datum formatter.
getDatumString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (d -> String)
{-# INLINE getDatumString #-}
getDatumString = getATMSMutable atmsDatumString
-- |Shortcut to write to the reference to a ATMS's datum formatter.
setDatumString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> (d -> String) -> ATMST s m ()
{-# INLINE setDatumString #-}
setDatumString = setATMSMutable atmsDatumString

-- |When the data associated with `Node`s are all `String`s, we can
-- direct the `ATMS` to display each datum as itself.
setDatumStringViaString :: Monad m => ATMS String i r s m -> ATMST s m ()
setDatumStringViaString atms = setDatumString atms id

-- |When the data associated with `Node`s are of a type of class
-- `Show`, we can direct the `ATMS` to display each datum using the
-- `show` instance.
setDatumStringViaShow ::
  (NodeDatum d, Show d, Monad m) => ATMS d i r s m -> ATMST s m ()
setDatumStringViaShow atms = setDatumString atms show

-- |Return the `ATMS`'s current informant formatter.
getInformantString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m (i -> String)
{-# INLINE getInformantString #-}
getInformantString = getATMSMutable atmsInformantString
-- |Shortcut to write to the reference to a ATMS's informant formatter.
setInformantString ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> (i -> String) -> ATMST s m ()
{-# INLINE setInformantString #-}
setInformantString = setATMSMutable atmsInformantString

-- |When the informants associated with `JustRule`s are all
-- `String`s, we can direct the `ATMS` to display each informant as
-- itself.
setInformantStringViaString ::
  (Monad m, NodeDatum d) => ATMS d String r s m -> ATMST s m ()
setInformantStringViaString atms = setInformantString atms id

-- |When the informants associated with `JustRule`s are of a type of
-- class `Show`, we can direct the `ATMS` to display each datum using
-- the `show` instance.
setInformantStringViaShow ::
  (Show i, Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
setInformantStringViaShow atms = setInformantString atms show

-- |Return the `ATMS`'s current rule-queueing procedure.
getEnqueueProcedure ::
  (Monad m, NodeDatum d) =>
    ATMS d i r s m -> ATMST s m (r -> ATMST s m ())
{-# INLINE getEnqueueProcedure #-}
getEnqueueProcedure = getATMSMutable atmsEnqueueProcedure
-- |Shortcut to write to the reference to a ATMS's rule-queueing procedure.
setEnqueueProcedure ::
  (Monad m, NodeDatum d) =>
    ATMS d i r s m -> (r -> ATMST s m ()) -> ATMST s m ()
{-# INLINE setEnqueueProcedure #-}
setEnqueueProcedure = setATMSMutable atmsEnqueueProcedure

-- |Print the internal title signifying an ATMS.
--
-- Translated from @print-atms@ in @atms.lisp@.
printAtms :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
printAtms atms = liftIO $ putStrLn $ "#<ATMS: " ++ atmsTitle atms ++ ">"

-- |Get the next node counter value, incrementing for future accesses.
nextNodeCounter :: (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m Int
nextNodeCounter jtms = sttLayer $ do
  let nodeCounter = atmsNodeCounter jtms
  nodeId <- readSTRef nodeCounter
  writeSTRef nodeCounter $ 1 + nodeId
  return nodeId

-- |Get the next justification rule counter value, incrementing for
-- future accesses.
nextJustCounter :: (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m Int
nextJustCounter atms = sttLayer $ do
  let justCounter = atmsJustCounter atms
  justId <- readSTRef justCounter
  writeSTRef justCounter $ 1 + justId
  return justId

-- |Get the next environment rule counter value, incrementing for
-- future accesses.
nextEnvCounter :: (Monad m, NodeDatum d) => ATMS d i r s m -> ATMST s m Int
nextEnvCounter atms = sttLayer $ do
  let envCounter = atmsEnvCounter atms
  envId <- readSTRef envCounter
  writeSTRef envCounter $ 1 + envId
  return envId

{- ----------------------------------------------------------------- -}

-- |Wrapper for the datum associated with a node of the `ATMS`.
--
-- Translated from @(tms-node@ in @atms.lisp@.
data (Monad m, NodeDatum d) => Node d i r s m = Node {
  nodeIndex :: Int,
  -- |Retrieve the datum associated with a `Node`.
  nodeDatum :: d,
  nodeLabel :: STRef s [Env d i r s m],
  nodeJusts :: STRef s [Justification d i r s m],
  nodeConsequences :: STRef s [JustRule d i r s m],
  nodeIsContradictory :: STRef s Bool,
  nodeIsAssumption :: STRef s Bool,
  nodeRules :: STRef s [r],
  -- |Retrieve the `ATMS` associated with a `Node`.
  nodeATMS :: ATMS d i r s m
}

instance (Monad m, NodeDatum d) => Eq (Node d i r s m) where
  n1 == n2 = nodeIndex n1 == nodeIndex n2

instance (Monad m, NodeDatum d) => Ord (Node d i r s m) where
  n1 < n2 = nodeIndex n1 < nodeIndex n2
  n1 `compare` n2 = nodeIndex n1 `compare` nodeIndex n2

instance (Monad m, NodeDatum d) => Show (Node d i r s m) where
  show n = "<Node " ++ show (nodeIndex n) ++ ">"

-- |Shortcut maker for reading from a `Node` reference.
getNodeMutable ::
  (Monad m, NodeDatum d) => (Node d i r s m -> STRef s a) -> Node d i r s m  -> ATMST s m a
{-# INLINE getNodeMutable #-}
getNodeMutable refGetter node = sttLayer $ readSTRef (refGetter node)
-- |Shortcut to write to the reference to a node's label.
setNodeMutable ::
  (Monad m, NodeDatum d) =>
    (Node d i r s m -> STRef s a) -> Node d i r s m -> a -> ATMST s m ()
{-# INLINE setNodeMutable #-}
setNodeMutable refGetter node val = sttLayer $ writeSTRef (refGetter node) val

-- |Return the `Node`'s label.
getNodeLabel ::
  (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m [Env d i r s m]
{-# INLINE getNodeLabel #-}
getNodeLabel = getNodeMutable nodeLabel
-- |Shortcut to write to the reference to a node's label.
setNodeLabel :: (Monad m, NodeDatum d) => Node d i r s m -> [Env d i r s m] -> ATMST s m ()
{-# INLINE setNodeLabel #-}
setNodeLabel = setNodeMutable nodeLabel

-- |Return the `Node`'s rules.
getNodeRules :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m [r]
{-# INLINE getNodeRules #-}
getNodeRules = getNodeMutable nodeRules
-- |Shortcut to write to the reference to a node's rules.
setNodeRules :: (Monad m, NodeDatum d) => Node d i r s m -> [r] -> ATMST s m ()
{-# INLINE setNodeRules #-}
setNodeRules = setNodeMutable nodeRules

-- |Return the `JustRule`s concluding a `Node`.
getNodeJusts ::
  (Monad m, NodeDatum d) =>
    Node d i r s m -> ATMST s m [Justification d i r s m]
{-# INLINE getNodeJusts #-}
getNodeJusts = getNodeMutable nodeJusts

-- |Return the `Node`'s consequences.
getNodeConsequences ::
  (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m [JustRule d i r s m]
{-# INLINE getNodeConsequences #-}
getNodeConsequences = getNodeMutable nodeConsequences
-- |Shortcut to write to the reference to a node's consequences.
setNodeConsequences ::
  (Monad m, NodeDatum d) => Node d i r s m -> [JustRule d i r s m] -> ATMST s m ()
{-# INLINE setNodeConsequences #-}
setNodeConsequences = setNodeMutable nodeConsequences

-- |Return whether the `Node`'s is currently contradictory.
getNodeIsContradictory :: (Monad m, NodeDatum d) => Node d i r s m  -> ATMST s m Bool
getNodeIsContradictory node = sttLayer $ readSTRef (nodeIsContradictory node)

-- |Return whether the `Node`'s is currently markable as an assumption.
getNodeIsAssumption :: (Monad m, NodeDatum d) => Node d i r s m  -> ATMST s m Bool
getNodeIsAssumption node = sttLayer $ readSTRef (nodeIsAssumption node)

-- |The justification of one `ATMS` `Node` by zero or more others.
data (Monad m, NodeDatum d) => JustRule d i r s m = JustRule {
  justIndex :: Int,
  -- |The informant associated with applying this inference rule.
  justInformant :: i,
  -- |The conclusion of this inference rule.
  justConsequence :: Node d i r s m,
  -- |The antecedents of this inference rule.
  justAntecedents :: [Node d i r s m]
}

-- |Description of why a `Node` may be believed by the `ATMS`.
data Justification d i r s m =
  ByRule (JustRule d i r s m) | ByAssumption (Node d i r s m) | ByContradiction

-- |Explanation of why a `Node` may be believed by the `ATMS` for
-- output to a query.
data Explanation d i r s m =
  IsRule (JustRule d i r s m) | IsAssumption (Node d i r s m)

-- |Explanation of why a `Node` may be classified as no-good by the
-- `ATMS`.
data WhyNogood d i r s m =
  Good | ByJustification (Justification d i r s m) | ByEnv (Env d i r s m)

-- |Translation of the explanation of why a `Node` may be classified
-- (or not) as no-good to a boolean value.
isNogood :: WhyNogood d i r s m -> Bool
isNogood Good = False
isNogood _ = True

{- ----------------------------------------------------------------- -}

-- |An environment of `Node`s which may be used as the basis of
-- reasoning in an `ATMS`.
data (Monad m, NodeDatum d) => Env d i r s m = Env {
  -- |The unique nomber of this `Env` within its `ATMS`.
  envIndex :: Int,
  -- |The number of assumptions contained within this `Env`.
  envCount :: Int,
  -- |The assumptions contained within this `Env`.
  envAssumptions :: [Node d i r s m],
  envNodes :: STRef s [Node d i r s m],
  envWhyNogood :: STRef s (WhyNogood d i r s m),
  envRules :: STRef s [r]
}

instance (Monad m, NodeDatum d) => Eq (Env d i r s m) where
  e1 == e2 = (envIndex e1) == (envIndex e2)

instance (Monad m, NodeDatum d) => Show (Env d i r s m) where
  show n = "<Env " ++ show (envIndex n) ++ ">"

-- |Shortcut maker for reading from a `Env` reference.
getEnvMutable ::
  (Monad m, NodeDatum d) => (Env d i r s m -> STRef s a) -> Env d i r s m  -> ATMST s m a
{-# INLINE getEnvMutable #-}
getEnvMutable refGetter env = sttLayer $ readSTRef (refGetter env)
-- |Shortcut to write to the reference to a env's label.
setEnvMutable ::
  (Monad m, NodeDatum d) =>
    (Env d i r s m -> STRef s a) -> Env d i r s m -> a -> ATMST s m ()
{-# INLINE setEnvMutable #-}
setEnvMutable refGetter env envs = sttLayer $ writeSTRef (refGetter env) envs

-- |Shortcut for reading the `Node`s of an `Env`.
getEnvNodes :: (Monad m, NodeDatum d) => Env d i r s m  -> ATMST s m [Node d i r s m]
getEnvNodes = getEnvMutable envNodes
-- |Shortcut for writing the `Node`s of an `Env`.
setEnvNodes :: (Monad m, NodeDatum d) => Env d i r s m  -> [Node d i r s m] -> ATMST s m ()
setEnvNodes = setEnvMutable envNodes

-- |Shortcut for reading the rules of an `Env`.
getEnvRules :: (Monad m, NodeDatum d) => Env d i r s m  -> ATMST s m [r]
getEnvRules = getEnvMutable envRules
-- |Shortcut for writing the rules of an `Env`.
setEnvRules :: (Monad m, NodeDatum d) => Env d i r s m  -> [r] -> ATMST s m ()
setEnvRules = setEnvMutable envRules

-- |Shortcut for testing whether an `Env` is nogood.
envIsNogood :: (Monad m, NodeDatum d) => Env d i r s m -> ATMST s m Bool
envIsNogood env = do
  fmap isNogood $ sttLayer $ readSTRef $ envWhyNogood env

-- |Type alias for the array storage of a table of `Env`s arranged by
-- length.
newtype EnvTable d i r s m = EnvTable (STArray s Int [Env d i r s m])

findInEnvTable ::
  (Monad m, NodeDatum d) =>
    (Env d i r s m -> Bool) -> EnvTable d i r s m ->
      ATMST s m (Maybe (Env d i r s m))
findInEnvTable pred (EnvTable arr) =
  let (lo, hi) = boundsSTArray arr
  in findInEnvTableEntries pred  [lo..hi] arr
  where findInEnvTableEntries ::
          Monad m =>
            (Env d i r s m -> Bool) -> [Int] -> STArray s Int [Env d i r s m] ->
              ATMST s m (Maybe (Env d i r s m))
        findInEnvTableEntries pred [] arr = return Nothing
        findInEnvTableEntries pred (i : idxs) arr = do
          entries <- sttLayer $ readSTArray arr i
          case find pred entries of
            Nothing -> findInEnvTableEntries pred idxs arr
            res -> return res

        findInEnvTableEntry ::
          Monad m =>
            (Env d i r s m -> Bool) -> [Env d i r s m] -> Maybe (Env d i r s m)
        findInEnvTableEntry pred envs = find pred envs

-- |Shortcut for retrieving the `Node` formatter from an `ATMS`, and
-- applying it to the given `Node`.
--
-- Translated from @node-string@ in @atms.lisp@.
nodeString :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m String
nodeString node = do
  nodeFmt <- getNodeString $ nodeATMS node
  return $ nodeFmt node

-- |Default formatter for the `Node`s of an `ATMS`.
--
-- Translated from @default-node-string@ in @atms.lisp@.
defaultNodeString ::
  (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m String
defaultNodeString node = do
  datumFormatter <- getDatumString $ nodeATMS node
  return $ datumFormatter $ nodeDatum node

-- |Insert an element into a sorted list.
--
-- Translated from @ordered-insert@ in @atms.lisp@.
orderedInsert :: Eq a => a -> [a] -> (a -> a -> Bool) -> [a]
orderedInsert item [] _ = [item]
orderedInsert item list@(i : _) test | test item i  = item : list
orderedInsert item list@(i : _) _    | item == i    = list
orderedInsert item (i : is) test = i : orderedInsert item is test

{- Does not seem to be used
-- Translated from @ordered-push@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defmacro ordered-push (item list test)
-- >   `(setq ,list (ordered-insert ,item ,list ,test)))
orderedPush :: a -> [a] -> (a -> a -> Bool) -> [a]
orderedPush = error "< unimplemented orderedPush >"
-}

-- |We order assumptions in `Env` lists by their index.
--
-- Translated from @assumptio-order@ in @atms.lisp@.
assumptionOrder :: (Monad m, NodeDatum d) => Node d i r s m -> Node d i r s m -> Bool
assumptionOrder n1 n2 = nodeIndex n1 < nodeIndex n2

-- Ordering predicate for two `Env`s; uses their internal index.
--
-- Translated from @env-order@ in @atms.lisp@.
envOrder :: (Monad m, NodeDatum d) => Env d i r s m -> Env d i r s m -> Bool
envOrder e1 e2 = envIndex e1 < envIndex e2

{- ----------------------------------------------------------------- -}

-- * Basic inference engine interface.

-- |Create a new, empty ATMS.
createATMS ::
  (Debuggable m, NodeDatum d) => String -> ATMST s m (ATMS d i r s m)
createATMS title = do
  ecInitialAlloc <- getInitialEnvTableAlloc
  emptyEnvRef <- sttLayer $ newSTRef Nothing
  contraNodeRef <- sttLayer $ newSTRef Nothing
  result <- sttLayer $ do
    nc <- newSTRef 0
    jc <- newSTRef 0
    ec <- newSTRef 0
    etAlloc <- newSTRef ecInitialAlloc
    nodes <- newSTRef ([] :: [Node d i r s m])
    justs <- newSTRef ([] :: [JustRule d i r s m])
    contradictions <- newSTRef ([] :: [Node d i r s m])
    assumptions <- newSTRef ([] :: [Node d i r s m])
    etable <- newSTArray (0, ecInitialAlloc) []
    etableRef <- newSTRef (EnvTable etable)
    ngtable <- newSTArray (0, ecInitialAlloc) []
    ngtableRef <- newSTRef (EnvTable ngtable)
    nodeString <- newSTRef (show . nodeIndex)
    justString <- newSTRef (show . justIndex)
    datumString <- newSTRef (\ datum -> "?")
    informantString <- newSTRef (\ inf -> "?")
    enqueueProcedure <- newSTRef (\ _ -> return ())
    debugging <- newSTRef False
    return $ ATMS title nc jc ec etAlloc
                  nodes justs contradictions assumptions
                  etableRef ngtableRef emptyEnvRef contraNodeRef
                  nodeString justString datumString informantString
                  enqueueProcedure debugging
  emptyEnv <- createEnv result []
  sttLayer $ writeSTRef emptyEnvRef (Just emptyEnv)
  contra <- createNode result contradictionNodeDatum False True
  sttLayer $ writeSTRef contraNodeRef (Just contra)
  return result

{- ----------------------------------------------------------------- -}

-- |Returns `True` if the given `Node` is axiomatic, following from
-- the assumption of zero other nodes.
--
-- Translated from @true-node?@ in @atms.lisp@.
isTrueNode :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m Bool
isTrueNode node = do
  envs <- getNodeLabel node
  return $ case envs of
    [] -> False
    e : _ -> null $ envAssumptions e

-- |Returns `True` if the given `Node` is justified by some labelling
-- `Env`ironment of `Node`s in the `ATMS`.
--
-- Translated from @in-node?@ in @atms.lisp@.
isInNode :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m Bool
isInNode node = fmap (not . null) (getNodeLabel node)

-- |Returns `True` if the given `Node` is justified by some subset of
-- the given environment in the `ATMS`.
--
-- Translated from @in-node?@ in @atms.lisp@.
isInNodeByEnv ::
  (Monad m, NodeDatum d) => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isInNodeByEnv node env = do
  labelEnvs <- getNodeLabel node
  return $ any (\ le -> isSubsetEnv le env) labelEnvs

-- |Returns `True` if the given `Node` is justified by no labelling
-- `Env`ironment of `Node`s in the `ATMS`.
--
-- Translated from @out-node?@ in @atms.lisp@.
isOutNode ::
  (Monad m, NodeDatum d) => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isOutNode node env = fmap not $ isInNodeByEnv node env

-- |Returns `True` if some environment justifying the given `Node` is
-- consistent with the given environment, where two environments are
-- consistent when their union is not no-good.
--
-- Translated from @node-consistent-with?@ in @atms.lisp@.
isNodeConsistentWith ::
  (Monad m, NodeDatum d) => Node d i r s m -> Env d i r s m -> ATMST s m Bool
isNodeConsistentWith node env = do
  labelEnvs <- getNodeLabel node
  anyByM (\ le -> do
             union <- unionEnv le env
             fmap not $ envIsNogood union)
    labelEnvs

-- |Create a new `Node` in an `ATMS`.
--
-- Translated from @create-node@ in @atms.lisp@.
createNode :: (Debuggable m, NodeDatum d) =>
  ATMS d i r s m -> d -> Bool -> Bool -> ATMST s m (Node d i r s m)
createNode atms datum isAssumption isContradictory = do
  idx <- nextNodeCounter atms
  label <- sttLayer $ newSTRef []
  justs <- sttLayer $ newSTRef []
  conseq <- sttLayer $ newSTRef []
  assumptionFlag <- sttLayer $ newSTRef isAssumption
  contraFlag <- sttLayer $ newSTRef isContradictory
  rules <- sttLayer $ newSTRef []
  let node = Node idx datum label justs conseq
                  contraFlag assumptionFlag rules atms
  sttLayer $ do
    push node $ atmsNodes atms
    when isContradictory $ push node $ atmsContradictions atms
  when isAssumption $ do
    selfEnv <- createEnv atms [node]
    sttLayer $ do
      push node $ atmsAssumptions atms
      push selfEnv $ nodeLabel node
  return node

-- |Mark the given `Node` as to be believed as an assumption by its
-- `ATMS`.
--
-- Translated from @assume-node@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun assume-node (node &aux atms)
-- >   (unless (tms-node-assumption? node)
-- >     (setq atms (tms-node-atms node))
-- >     (debugging atms  "~%Converting ~A into an assumption" node)
-- >     (setf (tms-node-assumption? node) t)
-- >     (push node (atms-assumptions atms))
-- >     (update (list (create-env atms (list node)))
-- >        node
-- >        'ASSUME-NODE)))
assumeNode :: (Debuggable m, NodeDatum d) => Node d i r s m -> ATMST s m ()
assumeNode node =
  unlessM (getNodeIsAssumption node) $ do
    let atms = nodeATMS node
     in do
      sttLayer $ push node (atmsAssumptions atms)
      selfEnv <- findOrMakeEnv [node] atms
      nodes <- sttLayer $ toMList [selfEnv]
      update nodes node (ByAssumption node)

-- |Mark the given `Node` as an additional contradiction node of the
-- `ATMS`.
--
-- TO BE TRANSLATED from @make-contradiction@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun make-contradiction
-- >        (node &aux (atms (tms-node-atms node)) nogood)
-- >   (unless (tms-node-contradictory? node)
-- >     (setf (tms-node-contradictory? node) t)
-- >     (push node (atms-contradictions atms))
-- >     (do nil (nil)
-- >       (if (setq nogood (car (tms-node-label node)))
-- >      (new-nogood atms nogood 'MAKE-CONTRADICTION)
-- >      (return nil)))))
makeContradiction :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m ()
makeContradiction = error "< TODO unimplemented makeContradiction >"

-- |Direct the `ATMS` to believe a particular `Node` when all of the
-- given list of `Node`s are also believed.  The first argument is the
-- informant associated with this inference.
--
-- Translated from @justify-node@ in @atms.lisp@.
justifyNode ::
  (Debuggable m, NodeDatum d) =>
    i -> Node d i r s m -> [Node d i r s m] -> ATMST s m ()
justifyNode informant consequence antecedents = do
  -- Retrieve the ATMS in which we are working
  let atms = nodeATMS consequence

  -- Number and create a new justification record.
  idx <- nextJustCounter atms
  let just = JustRule idx informant consequence antecedents

  -- Register the new justification with the node it can imply.
  sttLayer $ push (ByRule just) (nodeJusts consequence)

  -- Register the new justification with the nodes that can trigger
  -- it.
  sttLayer $ forM_ antecedents $ \node -> push just $ nodeConsequences node

  -- Register the new justification with the ATMS itself.
  sttLayer $ push just $ atmsJusts atms

  -- Introduce the new justification
  emptyEnv <- getEmptyEnvironment atms
  envListRef <- sttLayer $ fromListMap Just [emptyEnv]
  propagate just Nothing envListRef

-- |Direct the `ATMS` to find the combination of all of the given
-- `Node`s to be a contradiction associated with the given informant.
--
-- Translated from @nogood-nodes@ in @atms.lisp@.
nogoodNodes :: (Monad m, NodeDatum d) => i -> [Node d i r s m] -> ATMST s m ()
nogoodNodes informant nodes = do
  contra <- getContradictionNode (nodeATMS (head nodes))
  justifyNode informant contra nodes

-- * Label updating

--
-- Translated from @propagate@ in @atms.lisp@.
propagate ::
  (Debuggable m, NodeDatum d) =>
    JustRule d i r s m ->
      Maybe (Node d i r s m) ->
        MList s (Maybe (Env d i r s m)) ->
          ATMST s m ()
propagate just antecedent envs = do
  $(dbg [| debugPropagateArgs just antecedent envs |])
  newEnvs <- weave antecedent envs (justAntecedents just)
  when (not (mnull newEnvs)) $ do
    update newEnvs (justConsequence just) (ByRule just)

debugPropagateArgs ::
  (MonadIO m, NodeDatum d) =>
    JustRule d i r s m ->
      Maybe (Node d i r s m) ->
        MList s (Maybe (Env d i r s m)) ->
          ATMST s m ()
debugPropagateArgs justRule antecedent envs = do
  liftIO $ putStrLn "Calling propagate with"
  let atms = nodeATMS $ justConsequence justRule
  liftIO $ putStr ". Just: "
  debugJust justRule

  case antecedent of
    Just n -> debugNode n
    Nothing -> liftIO $ putStrLn ". No antecedent"

  envLen <- sttLayer $ mlength envs
  case envLen of
    0 -> liftIO $ putStrLn ". No envs"
    1 -> do
      liftIO $ putStrLn ". Env: "
      envm <- sttLayer $ mcar envs
      case envm of
        Nothing -> liftIO $ putStrLn "<nulled out>"
        Just env -> debugEnv env
    _ -> do
      liftIO $ putStrLn ". Envs:"
      mlistFor_ sttLayer envs $ \em -> do
        liftIO $ putStr "  . "
        case em of
          Just e -> debugEnv e
          Nothing -> liftIO $ putStrLn "<nulled out>"

--
-- Translated from @update@ in @atms.lisp@.
update ::
  (Debuggable m, NodeDatum d) =>
    MList s (Maybe (Env d i r s m)) ->
      Node d i r s m ->
        Justification d i r s m ->
          ATMST s m ()
update newEnvs consequence just = do
  $(dbg [| debugUpdateArgs newEnvs consequence just |])
  let atms = nodeATMS consequence

  -- If the consequence node is a contradiction, then all we need to
  -- do is mark all of the environments implying it as contradictory
  -- as well.
  ifM (getNodeIsContradictory consequence)
    (mlistFor_ sttLayer newEnvs $ \ envmaybe ->
        case envmaybe of
          Nothing -> return ()
          Just env -> newNogood atms env just) $

    -- Otherwise we propagate further.  If this step prunes out all
    -- `Env`s from the `newEnvs`, then we have nothing further to do.
    do revNewEnvs <- updateLabel consequence newEnvs
       newEnvsRef <- sttLayer $ newSTRef $ revNewEnvs
       ifM (sttLayer $ getMnull newEnvsRef) (return ()) $ do

         -- Process rules queued in the consequence.
         enqueuef <- getEnqueueProcedure atms
         forMM_ (getNodeRules consequence) $ enqueuef

         -- Propagate to the justification rules which might depend on
         -- this node.  If ever the new Env list we are accumulating is
         -- paired down to the empty list, then we can exit these loops.
         forMMwhile_ (getNodeConsequences consequence)
           (sttLayer $ notM $ getMnull newEnvsRef) $ \ supportedJust -> do
             currentNewEnvs <- sttLayer $ readSTRef newEnvsRef
             propagate supportedJust (Just consequence) newEnvs
             mlistForCons_ sttLayer newEnvs $ \ mcons -> do
               thisEnvMaybe <- sttLayer $ mcar mcons
               case thisEnvMaybe of
                 Just thisEnv -> do
                   label <- getNodeLabel consequence
                   unless (elem thisEnv label) $
                     sttLayer $ rplaca mcons Nothing
                 Nothing -> return ()
               cleanedNewEnvs <- sttLayer $ getMlistStripNothing newEnvsRef
               sttLayer $ writeSTRef newEnvsRef cleanedNewEnvs

debugUpdateArgs ::
  (MonadIO m, NodeDatum d) =>
    MList s (Maybe (Env d i r s m)) ->
      Node d i r s m ->
        JustRule d i r s m ->
          ATMST s m ()
debugUpdateArgs envs consequence justRule = do
  liftIO $ putStrLn "Calling update with"
  let atms = nodeATMS $ justConsequence justRule

  envLen <- sttLayer $ mlength envs
  case envLen of
    0 -> liftIO $ putStrLn ". No envs"
    1 -> do
      liftIO $ putStr ". Env: "
      envm <- sttLayer $ mcar envs
      case envm of
        Nothing -> liftIO $ putStrLn "<nulled out>"
        Just env -> debugEnv env
    _ -> do
      liftIO $ putStrLn ". Envs:"
      mlistFor_ sttLayer envs $ \em -> do
        liftIO $ putStr "  . "
        case em of
          Just e -> debugEnv e
          Nothing -> liftIO $ putStrLn "<nulled out>"

  liftIO $ putStr ". Consequence: "
  blurbNode consequence
  liftIO $ putStrLn ""

  liftIO $ putStr ". Just: "
  debugJust justRule

-- |Internal method to update the label of this node to include the
-- given environments.  The inclusion is not simply list extension;
-- new environments subsumed by an existing label environment will be
-- omitted, and existing label environments subsumed by a new
-- environment will be removed.
--
-- Translated from @update-label@ in @atms.lisp@.
updateLabel ::
  (Debuggable m, NodeDatum d) =>
    Node d i r s m -> MList s (Maybe (Env d i r s m)) ->
      ATMST s m (MList s (Maybe (Env d i r s m)))
updateLabel node newEnvs = do
  $(dbg [| debugUpdateLabelArgs node newEnvs |])

  -- We will edit the label of this node, so we extract it as a
  -- mutable list, and replace it at the end of this function.
  envsR <- do labels <- getNodeLabel node
              envs <- sttLayer $ fromListMap Just labels
              sttLayer $ newSTRef envs

  -- These two loops traverse respectively the given newEnvs, and the
  -- node label environments, to find pairs of environments where one
  -- of the pair is a subset of the other.
  mlistForCons_ sttLayer newEnvs $ \ newEnvCons -> do
    newEnvCarMaybe <- sttLayer $ mcar newEnvCons
    case newEnvCarMaybe of
      Nothing -> return ()
      Just newEnvCar -> do

        thisEnvs <- sttLayer $ readSTRef envsR
        mlistForCons_ sttLayer thisEnvs $ \ nenvCons -> do
          nenvCarMaybe <- sttLayer $ mcar nenvCons
          case nenvCarMaybe of
            Nothing -> return ()
            Just nenvCar -> do
               case compareEnv newEnvCar nenvCar of
                EQenv -> sttLayer $ rplaca newEnvCons Nothing
                S21env -> sttLayer $ rplaca newEnvCons Nothing
                S12env -> do
                  nodeList <- getEnvNodes nenvCar
                  setEnvNodes nenvCar $ delete node nodeList
                  sttLayer $ rplaca nenvCons Nothing
                DisjEnv -> return ()

        $(dbg [| do liftIO $ putStr " >> pushing onto envs: "
                    blurbMaybeEnv newEnvCarMaybe
                    liftIO $ putStrLn "" |])
        sttLayer $ mlistRefPush newEnvCarMaybe envsR
        $(dbg [| do liftIO $ putStr " >> envs: "
                    blurbMaybeEnvMListRef envsR
                    liftIO $ putStrLn "" |])
        return ()

  -- Strip all `Nothing`s from the `newEnvs`, and add the `node` to
  -- each environment's node list.
  finalNewEnvs <- sttLayer $ mlistStripNothing newEnvs
  mlistFor_ sttLayer finalNewEnvs $ \ newEnvMaybe ->
    case newEnvMaybe of
      Just newEnv -> sttLayer $ push node $ envNodes newEnv  -- [B]
      _ -> return ()

  -- Un-lift the working version of the node label list, and write the
  -- update back to the node label list.
  $(dbg [| do liftIO $ putStr " >> envs: "
              blurbMaybeEnvMListRef envsR
              liftIO $ putStrLn "" |])
  envs <- sttLayer $ readSTRef envsR
  updatedLabel <- sttLayer $ toUnmaybeList envs
  $(dbg [| do liftIO $ putStr " >> updatedLabel: "
              blurbEnvList 10000 "" updatedLabel
              liftIO $ putStrLn "" |])

  -- debugNodeLabel node
  -- sttLayer $ writeSTRef (nodeLabel node) updatedLabel
  setNodeLabel node updatedLabel
  -- debugNodeLabel node

  -- Return the Nothing-stripped version of the newEnvs parameter.
  $(dbg [| debugUpdateLabelFinal node updatedLabel finalNewEnvs |])
  return finalNewEnvs

debugUpdateLabelArgs ::
  (MonadIO m, NodeDatum d) =>
    Node d i r s m -> MList s (Maybe (Env d i r s m)) -> ATMST s m ()
debugUpdateLabelArgs node newEnvs = do
  let atms = nodeATMS node

  liftIO $ putStr "Calling updateLabel with node "
  blurbNode node
  liftIO $ putStrLn ""

  envLen <- sttLayer $ mlength newEnvs
  case envLen of
    0 -> liftIO $ putStrLn ". No envs"
    1 -> do
      liftIO $ putStr ". Env: "
      envm <- sttLayer $ mcar newEnvs
      case envm of
        Nothing -> liftIO $ putStrLn "<nulled out>"
        Just env -> debugEnv env
    _ -> do
      liftIO $ putStrLn ". Envs:"
      mlistFor_ sttLayer newEnvs $ \em -> do
        liftIO $ putStr "  . "
        case em of
          Just e -> debugEnv e
          Nothing -> liftIO $ putStrLn "<nulled out>"

debugUpdateLabelFinal ::
  (MonadIO m, NodeDatum d) =>
    Node d i r s m -> [Env d i r s m] -> MList s (Maybe (Env d i r s m)) ->
      ATMST s m ()
debugUpdateLabelFinal node labelEnvs newEnvs = do

  case labelEnvs of
    [] -> liftIO $ putStrLn ". No label envs"
    [env] -> do
      liftIO $ putStr ". Single label env: "
      debugEnv env
    _ -> do
      liftIO $ putStrLn ". Final envs:"
      forM_ labelEnvs $ \e -> do
        liftIO $ putStr "  . "
        debugEnv e

  envLen <- sttLayer $ mlength newEnvs
  case envLen of
    0 -> liftIO $ putStrLn ". No final envs"
    1 -> do
      liftIO $ putStr ". Single final env: "
      envm <- sttLayer $ mcar newEnvs
      case envm of
        Nothing -> liftIO $ putStrLn "<nulled out>"
        Just env -> debugEnv env
    _ -> do
      liftIO $ putStrLn ". Final envs:"
      mlistFor_ sttLayer newEnvs $ \em -> do
        liftIO $ putStr "  . "
        case em of
          Just e -> debugEnv e
          Nothing -> liftIO $ putStrLn "<nulled out>"

  debugNode node

-- |Update the label of node @antecedent@ to include the given @envs@
-- environments, pruning environments which are a superset of another
-- included enviroment.
--
-- Implements Algorithm 12.3 of /Building Problem Solvers/.
--
-- Translated from @weave@ in @atms.lisp@.
weave :: (Debuggable m, NodeDatum d) =>
  Maybe (Node d i r s m) ->
    (MList s (Maybe (Env d i r s m))) ->
      [Node d i r s m] ->
        ATMST s m (MList s (Maybe (Env d i r s m)))
weave antecedent givenEnvs antecedents = do
  $(dbg [| debugWeaveArgs antecedent givenEnvs antecedents |])

  envsRef <- sttLayer $ newSTRef givenEnvs

  forM_ antecedents $ \node ->
    unless (maybe False (node ==) antecedent) $ do
      $(dbg [| debugWeaveNodeAntecedent node |])

      -- From loop to loop we update what's stored under envsRef, so
      -- we start this outer loop by reading what we start off with
      -- there.
      envs <- sttLayer $ readSTRef envsRef

      -- We will update envs with the list built in newEnvs.
      newEnvs <- sttLayer $ newSTRef MNil

      -- We look at all pairs of
      --  - An Env from the passed-in ENVS, plus
      --  - An Env from the NODE's label.
      -- The union of these two is NEW-ENV, and the body of the loop
      -- considers how we should incorporate NEW-ENV into NEW-ENVS.
      mlistFor_ sttLayer envs $ \envmaybe ->
        case envmaybe of
          Nothing -> return ()
          Just env -> do
            forMM_ (sttLayer $ readSTRef $ nodeLabel node) $ \nodeEnv -> do
              $(dbg [| debugWeavePairIntro env nodeEnv |])

              newEnv <- unionEnv env nodeEnv
              $(dbg [| debugWeavePairUnion newEnv |])

              -- We are not interested in nogood environments, so we
              -- skip filing the union if it is nogood.
              unlessM (envIsNogood newEnv) $ do

                -- If NEW-ENV is a superset of (or is equal to)
                -- anything already in NEW-ENVS, then NEW-ENV is
                -- redundant, and we abort the body of the inner
                -- match-searching loop without adding NEW-ENV to
                -- NEW-ENVS.
                --
                -- Otherwise if anything already in NEW-ENVS is a
                -- superset of NEW-ENV, then (1) NEW-ENV makes that
                -- element redundant, and we strip it out of NEW-ENVS;
                -- and (2) we add NEW-ENV to NEW-ENVS.

                addEnv <- sttLayer $ newSTRef True

                oldMCons <- sttLayer $ readSTRef newEnvs
                mlistForConsWhile_ sttLayer oldMCons
                                   (sttLayer $ readSTRef addEnv) $ \ cons ->
                  case cons of
                    MNil -> return () -- Should not be possible
                    mc@(MCons carRef cdrRef) -> do
                      maybeCar <- sttLayer $ readSTRef carRef
                      case maybeCar of
                        Nothing -> return ()
                        Just car ->
                          case compareEnv newEnv car of
                            EQenv  -> sttLayer $ writeSTRef addEnv False
                            S12env -> do
                              $(dbg [| debugWeaveLoopRemovingEnv car |])
                              sttLayer $ rplaca cons Nothing
                            S21env -> sttLayer $ writeSTRef addEnv False
                            DisjEnv -> return ()

                -- If we haven't found newEnv to be redundant, then
                -- add it to newEnvs.
                sttLayer $ whenM (readSTRef addEnv) $ do
                  newMCons <- mlistPush (Just newEnv) oldMCons
                  writeSTRef newEnvs newMCons
                $(dbg [| debugWeaveLoopPairEnd addEnv newEnvs |])

      -- So we have nearly produced the refinement of ENVS for this
      -- NODE in the ANTECEDENTS.  It might have spurious NILs, so we
      -- strip those out and update envsRef.
      preFinalNewEnvs <- sttLayer $ readSTRef newEnvs
      filteredNewEnvs <- sttLayer $ mlistStripNothing preFinalNewEnvs
      sttLayer $ writeSTRef envsRef filteredNewEnvs

  -- Finally, return the last refinement of ENVS.
  result <- sttLayer $ readSTRef envsRef
  $(dbg [| debugWeaveResult result |])
  return result

debugWeaveArgs :: (MonadIO m, NodeDatum d) =>
  Maybe (Node d i r s m) ->
    (MList s (Maybe (Env d i r s m))) ->
      [Node d i r s m] ->
        ATMST s m ()
debugWeaveArgs antecedent givenEnvs antecedents = do
  liftIO $ putStrLn "Calling weave with"
  case antecedent of
    Just n -> debugNode n
    Nothing -> liftIO $ putStrLn ". No antecedent"
  let atms = case antecedent of
               Just a  -> Just $ nodeATMS a
               Nothing -> case antecedents of
                            a : _ -> Just $ nodeATMS a
                            _ -> Nothing
  case atms of
    Just a -> do
      liftIO $ putStrLn ". Envs:"
      mlistFor_ sttLayer givenEnvs $ \em -> do
        liftIO $ putStr "  . "
        case em of
          Just e -> debugEnv e
          Nothing -> liftIO $ putStrLn "<nulled out>"
        return ()
    _ -> return ()

  liftIO $ putStr ". Antecedents:"
  forM_ antecedents $ \a -> do
    let atms = nodeATMS a
    datumFmt <- getDatumString atms
    liftIO $ putStr $ " " ++ datumFmt (nodeDatum a)
  liftIO $ putStrLn " "

debugWeaveNodeAntecedent antecedent = do
  datumFmt <- getDatumString (nodeATMS antecedent)
  liftIO $ putStrLn $
    " - For node antecedent " ++ datumFmt (nodeDatum antecedent)

debugWeavePairIntro ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> Env d i r s m -> ATMST s m ()
debugWeavePairIntro srcEnv nodeEnv = do
  liftIO $ putStr $ "    - For "
  blurbEnv srcEnv
  liftIO $ putStr $ " from env, "
  blurbEnv nodeEnv
  liftIO $ putStrLn $ " from node label"

debugWeavePairUnion ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
debugWeavePairUnion union = do
  liftIO $ putStr "      Union is "
  blurbEnv union
  liftIO $ putStrLn ""

debugWeaveResult ::
  (MonadIO m, NodeDatum d) => MList s (Maybe (Env d i r s m)) -> ATMST s m ()
debugWeaveResult result = do
  liftIO $ putStr " --> result of weave is "
  blurbMaybeEnvMList result
  liftIO $ putStrLn ""

debugWeaveLoopRemovingEnv ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
debugWeaveLoopRemovingEnv env = do
  liftIO $ putStr "       - Removing from result: env "
  blurbEnv env
  liftIO $ putStrLn ""

debugWeaveLoopPairEnd ::
  (MonadIO m, NodeDatum d) =>
    STRef s Bool -> (STRef s (MList s (Maybe (Env d i r s m)))) -> ATMST s m ()
debugWeaveLoopPairEnd addR envmsR = do
  add <- sttLayer $ readSTRef addR
  liftIO $ putStrLn $ "      Adding union: " ++ (if add then "yes" else "no")
  mlist <- sttLayer $ readSTRef envmsR
  liftIO $ putStr $ "      Updated result to: "
  blurbMaybeEnvMList mlist
  liftIO $ putStrLn ""

-- Translated from @in-antecedent?@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun in-antecedent? (nodes)
-- >   (or (null nodes)
-- >       (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))
isInAntecedent :: (Monad m, NodeDatum d) => [Node d i r s m] -> ATMST s m Bool
isInAntecedent [] = return True
isInAntecedent nodes = do
  empty <- getEmptyEnvironment (nodeATMS (head nodes))
  isWeave empty nodes

-- |TO BE TRANSLATED from @weave?@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun weave? (env nodes &aux new-env)
-- >   (cond ((null nodes) t)
-- >    (t (dolist (e (tms-node-label (car nodes)))
-- >         (setq new-env (union-env e env))
-- >         (unless (env-nogood? new-env)
-- >           (if (weave? new-env (cdr nodes))
-- >               (return T)))))))
isWeave :: (Monad m, NodeDatum d) => Env d i r s m -> [Node d i r s m] -> ATMST s m Bool
isWeave = error "< TODO unimplemented isInAntecedent >"

-- |TO BE TRANSLATED from @supporting-antecedent?@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun supporting-antecedent? (nodes env)
-- >   (dolist (node nodes t) (unless (in-node? node env) (return nil))))
isSupportingAntecedent ::
  (Monad m, NodeDatum d) => [Node d i r s m] -> Env d i r s m -> ATMST s m Bool
isSupportingAntecedent = error "< TODO unimplemented isSupportingAntecedent >"

-- |Remove a `Node` from the `ATMS`.
--
-- TO BE TRANSLATED from @remove-node@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun remove-node (node &aux atms)
-- >   (if (tms-node-consequences node)
-- >       (error "Can't remove node with consequences"))
-- >   (setq atms (tms-node-atms node))
-- >   (setf (atms-nodes atms)
-- >    (delete node (atms-nodes atms) :test #'eq :count 1))
-- >   (dolist (just (tms-node-justs node))
-- >     (dolist (ant (just-antecedents just))
-- >       (setf (tms-node-consequences ant)
-- >        (delete just (tms-node-consequences ant)
-- >                :test #'eq :count 1))))
-- >   (dolist (env (tms-node-label node))
-- >     (setf (env-nodes env)
-- >      (delete node (env-nodes env) :test #'eq :count 1))))
removeNode :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m ()
removeNode = error "< TODO unimplemented removeNode >"

-- * Creating and extending environments.

-- |Create and return a new `Env` for the given assumptions.  Note
-- that this function does not sort or otherwise organize
-- @assumptions@, and it only called with an empty or singleton list.
-- Instead, it is `consEnv` which inserts nodes in order when one
-- environement is defined in terms of another.
--
-- Translated from @create-env@ in @atms.lisp@.
createEnv ::
  (Debuggable m, NodeDatum d) =>
    ATMS d i r s m -> [Node d i r s m] -> ATMST s m (Env d i r s m)
createEnv atms assumptions = do
  $(dbg [| debugCreateEnvStart assumptions |])
  index <- nextEnvCounter atms
  whyNogood <- sttLayer $ newSTRef Good
  nodes <- sttLayer $ newSTRef []
  rules <- sttLayer $ newSTRef []
  let env = Env index (length assumptions) assumptions nodes whyNogood rules
  $(dbg [| debugCreateEnvEnv env |])
  insertInTable atms (atmsEnvTable atms) env
  $(dbg [| debugCreateEnvEnv env |])
  setEnvContradictory atms env
  $(dbg [| debugCreateEnvEnv env |])
  return env

debugCreateEnvStart ::
  (MonadIO m, NodeDatum d) => [Node d i r s m] -> ATMST s m ()
debugCreateEnvStart nodes = do
  liftIO $ putStrLn $ "             - Running createEnv"
  astr <- formatNodes "," nodes
  liftIO $ putStrLn $ "               assumptions " ++ astr

debugCreateEnvEnv ::
  (MonadIO m, NodeDatum d) => (Env d i r s m) -> ATMST s m ()
debugCreateEnvEnv env = do
  liftIO $ putStr $ "               env "
  blurbEnv env
  liftIO $ putStrLn ""

-- Translated from @union-env@ in @atms.lisp@.
unionEnv ::
  (Debuggable m, NodeDatum d) =>
    Env d i r s m -> Env d i r s m -> ATMST s m (Env d i r s m)
unionEnv e1 e2 =
  if envCount e1 > envCount e2 then unionEnv' e2 e1 else unionEnv' e1 e2
  where unionEnv' e1 e2 = do
          $(dbg [| debugUnionEnvStart e1 e2 |])
          acc <- sttLayer $ newSTRef e2
          forMwhile_ (envAssumptions e1)
                     (do thisE2 <- sttLayer $ readSTRef acc
                         notM $ envIsNogood thisE2) $ \assume -> do
            oldE2 <- sttLayer $ readSTRef acc
            $(dbg [| debugUnionEnvLoopStart assume oldE2 |])
            newE2 <- consEnv assume oldE2
            $(dbg [| debugUnionEnvLoopCons newE2 |])
            sttLayer $ writeSTRef acc newE2
          result <- sttLayer $ readSTRef acc
          $(dbg [| debugUnionEnvResult result |])
          return result

debugUnionEnvStart ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> Env d i r s m -> ATMST s m ()
debugUnionEnvStart e1 e2 = do
  liftIO $ putStr "       - Starting unionEnv' with "
  blurbEnv e1
  liftIO $ putStr "; "
  blurbEnv e2
  liftIO $ putStrLn ""

debugUnionEnvLoopStart ::
  (MonadIO m, NodeDatum d) => Node d i r s m -> Env d i r s m -> ATMST s m ()
debugUnionEnvLoopStart node e2 = do
  datumFmt <- getDatumString $ nodeATMS node
  liftIO $ putStrLn $ "         - Running loop with"
  liftIO $ putStrLn $ "           node " ++ datumFmt (nodeDatum node)
  liftIO $ putStr "           env "
  blurbEnv e2
  liftIO $ putStrLn ""

debugUnionEnvLoopCons ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
debugUnionEnvLoopCons e = do
  liftIO $ putStr "           consEnv returns "
  blurbEnv e
  liftIO $ putStrLn ""

debugUnionEnvResult ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
debugUnionEnvResult result = do
  liftIO $ putStr "         unionEnv returns "
  blurbEnv result
  liftIO $ putStrLn ""


-- |Derive an environment from the addition of one additional
-- assumption to a previous `Env`'s assumption list.
consEnv ::
  (Debuggable m, NodeDatum d) =>
    Node d i r s m -> Env d i r s m -> ATMST s m (Env d i r s m)
consEnv assumption env = do
  $(dbg [| debugConsEnvStart assumption env |])

  let nassumes = orderedInsert assumption (envAssumptions env) assumptionOrder
  $(dbg [| debugConsEnvInserted nassumes |])

  envByLookup <- lookupEnv nassumes
  $(dbg [| debugConsEnvLookup envByLookup |])
  maybe (createEnv (nodeATMS assumption) nassumes) (return . id) envByLookup

debugConsEnvStart ::
  (MonadIO m, NodeDatum d) => Node d i r s m -> Env d i r s m -> ATMST s m ()
debugConsEnvStart node e2 = do
  datumFmt <- getDatumString $ nodeATMS node
  liftIO $ putStrLn $ "         - Running consEnv"
  liftIO $ putStrLn $ "           inserting node " ++ datumFmt (nodeDatum node)
  liftIO $ putStr "           into env "
  blurbEnv e2
  liftIO $ putStrLn ""

debugConsEnvInserted ::
  (MonadIO m, NodeDatum d) => [Node d i r s m] -> ATMST s m ()
debugConsEnvInserted nodes =
  case nodes of
    [] -> liftIO $ putStrLn "           list after insertion: empty list"
    (n : _) -> do
      datumFmt <- getDatumString $ nodeATMS n
      liftIO $ putStrLn $
        "           list after insertion: ["
        ++ intercalate ", " (map (datumFmt . nodeDatum) nodes)
        ++ "]"

debugConsEnvLookup ::
  (MonadIO m, NodeDatum d) => Maybe (Env d i r s m) -> ATMST s m ()
debugConsEnvLookup Nothing =
  liftIO $ putStrLn $ "           lookup gives Nothing"
debugConsEnvLookup (Just env) = do
  liftIO $ putStr $ "           lookup gives "
  blurbEnv env
  liftIO $ putStrLn ""

-- |TO BE TRANSLATED from @find-or-make-env@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun find-or-make-env (assumptions atms)
-- >   (unless assumptions
-- >     (return-from find-or-make-env (atms-empty-env atms)))
-- >   ;; Presumes the list of assumptions is ordered properly
-- >   (or (lookup-env assumptions)
-- >       (create-env atms assumptions)))
findOrMakeEnv ::
  (Monad m, NodeDatum d) =>
    [Node d i r s m] -> ATMS d i r s m -> ATMST s m (Env d i r s m)
findOrMakeEnv = error "< TODO unimplemented findOrMakeEnv >"

-- * Env tables.

-- Translated from @insert-in-table@ in @atms.lisp@.
insertInTable ::
  (Monad m, NodeDatum d) =>
    ATMS d i r s m -> STRef s (EnvTable d i r s m) -> Env d i r s m ->
      ATMST s m ()
insertInTable atms tableRef env = do
  let count = envCount env
  EnvTable currentTable <- sttLayer $ readSTRef tableRef
  let (_, alloc) = boundsSTArray currentTable

  -- Re-allocate the array if it needs to grow, and update the
  -- reference.
  when (alloc < count) $ do
    incr <- getEnvTableIncr
    let newAlloc = count + incr
    sttLayer $ do
      newArray <- newSTArray (0, newAlloc) []
      forM_ [1..alloc] $ \i -> do
        envs <- readSTArray currentTable i
        writeSTArray newArray i envs
      writeSTRef tableRef $ EnvTable newArray

  -- Add the env to its slot in the table.
  sttLayer $ do
    EnvTable array <- readSTRef tableRef
    oldEnvs <- readSTArray array count
    writeSTArray array count $ env : oldEnvs

-- Translated from @lookup-env@ in @atms.lisp@.
lookupEnv :: (Monad m, NodeDatum d) => [Node d i r s m] -> ATMST s m (Maybe (Env d i r s m))
lookupEnv [] = return Nothing
lookupEnv assumptions@(a : _) = do
  let atms = nodeATMS a
      ns = sortOn nodeIndex assumptions
  EnvTable envTable <- sttLayer $ readSTRef $ atmsEnvTable atms
  entries <- sttLayer $ readSTArray envTable $ length ns
  case filter (\x -> envAssumptions x == ns) entries of
    [] -> return Nothing
    (x : _) -> return $ Just x

-- Translated from @subset-env?@ in @atms.lisp@.
isSubsetEnv :: (Monad m, NodeDatum d) => Env d i r s m -> Env d i r s m -> Bool
isSubsetEnv e1 e2 =
  if e1 == e2 then True
  else if envCount e1 > envCount e2 then False
  else ordSubsetp (envAssumptions e1) (envAssumptions e2)

-- |The possible results of comparing two `Env`s.
data EnvCompare =
  EQenv     -- ^ Two `Env`s are the same
  | S12env  -- ^ The first `Env` is a subset of the second.
  | S21env  -- ^ The second `Env` is a subset of the first.
  | DisjEnv -- ^ Two `Env`s are disjoint.

-- Translated from @compare-env@ in @atms.lisp@.
compareEnv ::
  (Monad m, NodeDatum d) => Env d i r s m -> Env d i r s m -> EnvCompare
compareEnv e1 e2 =
  if e1 == e2
  then EQenv
  else if envCount e1 < envCount e2
       then if nodeListIsSubsetEq (envAssumptions e1) (envAssumptions e2)
            then S12env
            else DisjEnv
       else if nodeListIsSubsetEq (envAssumptions e2) (envAssumptions e1)
            then S21env
            else DisjEnv

-- |Return true if the first sorted (by `Env` index) node list is a
-- subset of the second.
nodeListIsSubsetEq :: (Monad m, NodeDatum d) => [Node d i r s m] -> [Node d i r s m] -> Bool
nodeListIsSubsetEq [] _ = True
nodeListIsSubsetEq _ [] = False
nodeListIsSubsetEq l1@(x : xs) (y : ys) =
  case nodeIndex x `compare` nodeIndex y of
    LT -> False
    EQ -> nodeListIsSubsetEq xs ys
    GT -> nodeListIsSubsetEq l1 ys
-- * Processing nogoods

-- Translated from @new-nogood@ in @atms.lisp@.
newNogood ::
  (Debuggable m, NodeDatum d) =>
    ATMS d i r s m -> Env d i r s m -> Justification d i r s m -> ATMST s m ()
newNogood atms cenv why = do
  $(dbg [| debugNewNogoodStart cenv why |])

  -- Record in `cenv` the reason why `cenv` is nogood.
  sttLayer $ writeSTRef (envWhyNogood cenv) (ByJustification why)

  -- `cenv` can no longer be used in node labels, so remove it from
  -- any node labels in which it appears, and propagate out any
  -- changes.
  removeEnvFromLabels cenv atms

  -- Add `cenv` to the ATMS table of nogoods.
  insertInTable atms (atmsNogoodTable atms) cenv

  -- Remove any nogood table entries made redundant by `cenv`.
  let cenvCount = envCount cenv
  EnvTable nogoodTable <- getNogoodTable atms
  forM_ [1 .. cenvCount - 1] $ \ i -> do
    entry <- sttLayer $ readSTArray nogoodTable i
    sttLayer $ writeSTArray nogoodTable i $
      filter (not . isSubsetEnv cenv) entry

  -- Find currently-non-nogood environments which are supersets of the
  -- nogood, and process them as nogoods.
  EnvTable envTable <- getEnvTable atms
  let (_, maxCount) = boundsSTArray envTable
  forM_ [cenvCount + 1, maxCount] $ \ i -> do
    entry <- sttLayer $ readSTArray envTable i
    forM_ entry $ \ old -> do
      isNogood <- envIsNogood old
      when (isNogood && isSubsetEnv cenv old) $ do
        sttLayer $ writeSTRef (envWhyNogood old) (ByEnv cenv)
        removeEnvFromLabels old atms

debugNewNogoodStart ::
  (MonadIO m, NodeDatum d) =>
    Env d i r s m -> Justification d i r s m -> ATMST s m ()
debugNewNogoodStart cenv why = do
  liftIO $ putStr "Starting newNogood with "
  debugEnv cenv
  formatJustification why >>= (liftIO . putStrLn)


-- Translated from @set-env-contradictory@ in @atms.lisp@.
setEnvContradictory ::
  (Debuggable m, NodeDatum d) => ATMS d i r s m -> Env d i r s m -> ATMST s m ()
setEnvContradictory atms env = do
  $(dbg [| setEnvContradictoryStart env |])
  ifM (envIsNogood env)
    (do $(dbg [| liftIO $ putStr "                 Already nogood \n" |])
        return ()) $ do
    let count = envCount env
    EnvTable nogoodTableArray <- sttLayer $ readSTRef $ atmsNogoodTable atms
    forM_ [1..count] $ \i -> do
      continueLoop <- sttLayer $ newSTRef True
      $(dbg [| setEnvContradictoryStartOuter i |])
      forMMwhile_ (sttLayer $ readSTArray nogoodTableArray i)
                  (sttLayer $ readSTRef continueLoop) $ \cenv -> do
        $(dbg [| setEnvContradictoryStartInner cenv |])
        when (isSubsetEnv cenv env) $ do
          $(dbg [| setEnvContradictoryStartInnerWhen cenv env |])
          sttLayer $ do
            writeSTRef (envWhyNogood env) $ ByEnv cenv
            writeSTRef continueLoop False

setEnvContradictoryStart ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
setEnvContradictoryStart e = do
  liftIO $ putStr "               - Running setEnvContradictory with "
  blurbEnv e
  liftIO $ putStrLn ""

setEnvContradictoryStartOuter ::
  (MonadIO m) => Int -> ATMST s m ()
setEnvContradictoryStartOuter i = do
  liftIO $ putStrLn $ ("                 Starting outer loop for "
                       ++ show i
                       ++ "-length envs")

setEnvContradictoryStartInner ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
setEnvContradictoryStartInner cenv = do
  liftIO $ putStr "                   Starting inner loop with nogood env "
  blurbEnv cenv
  liftIO $ putStrLn ""

setEnvContradictoryStartInnerWhen ::
  (MonadIO m, NodeDatum d) => Env d i r s m -> Env d i r s m -> ATMST s m ()
setEnvContradictoryStartInnerWhen cenv env = do
  liftIO $ putStr "                   Nogood "
  blurbEnv cenv
  liftIO $ putStr " is subset of "
  blurbEnv env
  liftIO $ putStrLn ", marking latter nogood"

-- Translated from @remove-env-from-labels@ in @atms.lisp@.
removeEnvFromLabels ::
  (Monad m, NodeDatum d) => Env d i r s m -> ATMS d i r s m -> ATMST s m ()
removeEnvFromLabels env atms = do
  -- Run all rules associated with `env`, and clear the list of
  -- associated rules.
  enqueuef <- getEnqueueProcedure atms
  forMM_ (getEnvRules env) $ \ rule -> do
    enqueuef rule
  setEnvRules env []

  -- Remove `env` from the label of the nodes currently including it.
  forMM_ (getEnvNodes env) $ \ node -> do
    oldLabel <- getNodeLabel node
    setNodeLabel node $ delete env oldLabel

-- * Interpretation construction

-- |Return the minimum environments which give the `ATMS` belief in
-- the given choice sets.  The choice sets are essentially
-- conjunctive-normal form expressions; in the list of sublists of
-- nodes, under each environment in the result at least one node of
-- each sublist will be believed.
--
-- TO BE TRANSLATED from @interpretations@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (proclaim '(special *solutions*))
-- > (defun interpretations (atms choice-sets &optional defaults
-- >                    &aux solutions)
-- >   (if (atms-debugging atms)
-- >    (format *trace-output*
-- >       "~%Constructing interpretations depth-first for ~a:" choice-sets))
-- >   (format *trace-output* "~%- Refining choice sets")
-- >   (let ((*solutions* nil)
-- >    (choice-sets
-- >      (mapcar #'(lambda (alt-set)
-- >                  (format *trace-output*
-- >                      "~%  - ~a --> ???" alt-set)
-- >                  (let ((result
-- >                         (mapcan #'(lambda (alt)
-- >                                     (format *trace-output*
-- >                                         "~%    - ~a --> ~a"
-- >                                         alt (tms-node-label alt))
-- >                                     (copy-list (tms-node-label alt)))
-- >                                 alt-set)))
-- >                    (format *trace-output*
-- >                        "~%    ~a --> ~a" alt-set result)
-- >                    result))
-- >              choice-sets)))
-- >     (format *trace-output* "~%  Refined choice sets to ~a" choice-sets)
-- >     (dolist (choice (car choice-sets))
-- >       (format *trace-output*
-- >      "~%- Calling depth-solutions with choice ~a" choice)
-- >       (format *trace-output*
-- >      "~%                               choice sets ~a" (car choice-sets))
-- >       (get-depth-solutions1 choice (cdr choice-sets))
-- >       (format *trace-output*
-- >      "~%      => solutions ~a" *solutions*))
-- >     (setq *solutions* (delete nil *solutions* :TEST #'eq))
-- >     (unless *solutions*
-- >       (if choice-sets (return-from interpretations nil)
-- >                  (setq *solutions* (list (atms-empty-env atms)))))
-- >     (when defaults
-- >       (setq solutions *solutions* *solutions* nil)
-- >       (dolist (solution solutions)
-- >    (extend-via-defaults solution defaults defaults)))
-- >     (delete nil *solutions* :TEST #'eq)))
interpretations ::
  (Monad m, NodeDatum d) => ATMS d i r s m -> [[Node d i r s m]] -> ATMST s m ()
interpretations = error "< TODO unimplemented interpretations >"

-- |TO BE TRANSLATED from @get-depth-solutions1@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun get-depth-solutions1 (solution choice-sets
-- >                                  &aux new-solution)
-- >   (cond ((null choice-sets)
-- >     (unless (do ((old-solutions *solutions* (cdr old-solutions)))
-- >                 ((null old-solutions))
-- >               (when (car old-solutions)
-- >                 (case (compare-env (car old-solutions) solution)
-- >                   ((:EQ :S12) (return t))
-- >                   (:S21 (rplaca old-solutions nil)))))
-- >       (push solution *solutions*)))
-- >    ((env-nogood? solution)) ;something died.
-- >    (t (dolist (choice (car choice-sets))
-- >         (setq new-solution (union-env solution choice))
-- >         (unless (env-nogood? new-solution)
-- >           (get-depth-solutions1 new-solution
-- >                                 (cdr choice-sets)))))))
getDepthSolutions1 ::
  (Monad m, NodeDatum d) => Env d i r s m -> [[Env d i r s m]] -> ATMST s m ()
getDepthSolutions1 = error "< TODO unimplemented getDepthSolutions1 >"

-- |TO BE TRANSLATED from @extend-via-defaults@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun extend-via-defaults (solution remaining original)
-- >   (do ((new-solution)
-- >        (defaults remaining (cdr defaults)))
-- >       ((null defaults)
-- >        (or (member solution *solutions* :TEST #'eq)
-- >       (dolist (default original)
-- >         (or (member default (env-assumptions solution)
-- >                     :TEST #'eq)
-- >             (env-nogood? (cons-env default solution))
-- >             (return t)))
-- >       (push solution *solutions*)))
-- >     (setq new-solution (cons-env (car defaults) solution))
-- >     (unless (env-nogood? new-solution)
-- >       (extend-via-defaults new-solution (cdr defaults) original))))
extendViaDefaults ::
  (Monad m, NodeDatum d) =>
    Env d i r s m -> [Node d i r s m] -> [Node d i r s m] -> ATMST s m ()
extendViaDefaults = error "< TODO unimplemented extendViaDefaults >"

-- * Generating explanations

-- |This function returns a list of justifications which form a
-- directed acyclic graph (DAG) for the derivation. This is quite
-- complicated because this is really a simple consequent JTMS.
--
-- Translated from @explain-node@ in @atms.lisp@.
explainNode ::
  (Monad m, NodeDatum d) =>
    Node d i r s m -> Env d i r s m -> ATMST s m [Explanation d i r s m]
explainNode node env = explainNode1 env node [] []

-- Translated from @explain-node-1@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun explain-node-1 (env node queued-nodes explanation)
-- >   (cond ((member node queued-nodes) nil)
-- >    ((and (tms-node-assumption? node)
-- >          (member node (env-assumptions env)))
-- >     (cons (cons 'ASSUME node) explanation))
-- >    ((dolist (just explanation)
-- >       (if (if (listp just)
-- >               (eq (cdr just) node) (eq (just-consequence just) node))
-- >           (return explanation))))
-- >    (t (setq queued-nodes (cons node queued-nodes))
-- >       (dolist (just (tms-node-justs node))
-- >         (unless (dolist (a (just-antecedents just))
-- >                   (unless (in-node? a env) (return t)))
-- >          (let ((new-explanation explanation))
-- >            (dolist (a (just-antecedents just)
-- >                       (return-from explain-node-1
-- >                         (cons just new-explanation)))
-- >              (setq new-explanation
-- >                    (explain-node-1 env a queued-nodes new-explanation))
-- >              (unless new-explanation (return nil)))))))))
explainNode1 ::
  (Monad m, NodeDatum d) =>
    Env d i r s m -> Node d i r s m -> [Node d i r s m] ->
      [Justification d i r s m] ->
        ATMST s m [Explanation d i r s m]
explainNode1 = error "< TODO unimplemented explainNode1 >"

-- |Print the justifying `Env`ironments which label a `Node`.
--
-- TO BE TRANSLATED from @why-node@ in @atms.lisp@.
--
-- > ;;; Printing
-- > (defun why-node (node &optional (stream t) (prefix ""))
-- >   (format stream "~%<~A~A,{" prefix (tms-node-datum node))
-- >   (dolist (e (tms-node-label node))
-- >     (env-string e stream))
-- >   (format stream "}>"))
whyNode :: (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m (Node d i r s m)
whyNode = error "< TODO unimplemented whyNode >"

-- |Print the justifying `Env`ironments which label each `Node` of an
-- `ATMS`.
--
-- Translated from @why-nodes@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun why-nodes (atms &optional (stream t))
-- >   (dolist (n (reverse (atms-nodes atms))) (why-node n stream)))
whyNodes :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
whyNodes atms = do
  nodes <- getNodes atms
  forM_ nodes whyNode

-- |Print a `Node`'s justifications.
--
-- TO BE TRANSLATED from @node-justifications@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun node-justifications (node &optional (stream t))
-- >   (format t "~% For ~A:" (node-string node))
-- >   (dolist (j (tms-node-justs node))
-- >     (print-justification j stream)))
nodeJustifications :: (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m ()
nodeJustifications node = do
  nodeStr <- nodeString node
  liftIO $ putStr $ " For " ++ nodeStr ++ ":"
  justs <- getNodeJusts node
  forM_ justs printJustification
  error "< TODO unimplemented nodeJustifications >"

-- |Retrieve an `ATMS`'s `Env`ironment with the given index number.
--
-- Translated from @e@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun e (atms n)
-- >   (dolist (bucket (atms-env-table atms))
-- >     (dolist (env (cdr bucket))
-- >     (if (= (env-index env) n) (return-from e env)))))
e :: (Monad m, NodeDatum d) =>
  ATMS d i r s m -> Int -> ATMST s m (Maybe (Env d i r s m))
e atms i = do
  table <- getEnvTable atms
  findInEnvTable (\env -> envIndex env == i) table

-- |Print an environment.
--
-- TO BE TRANSLATED from @print-env@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-env (e &optional (stream t))
-- >   (format stream "~%~A:~A"
-- >      e (if (env-nogood? e)
-- >            "* " " "))
-- >   (env-string e stream))
printEnv :: (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
printEnv = error "< TODO unimplemented printEnv >"

-- |Convert an `Env`ironment into a string listing the nodes of the
-- environment.
--
-- TO BE TRANSLATED from @env-string@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun env-string (e &optional stream
-- >                      &aux assumptions strings printer)
-- >   (setq assumptions (env-assumptions e))
-- >   (when assumptions
-- >     (setq printer (atms-node-string (tms-node-atms (car assumptions)))))
-- >   (dolist (a assumptions) (push (funcall printer a) strings))
-- >   (format stream "{~{~A~^,~}}" (sort strings #'string-lessp)))
envString :: (Monad m, NodeDatum d) => Env d i r s m -> ATMST s m String
envString = error "< TODO unimplemented envString >"

-- * Printing global data

-- |List the nogood `Env`ironments of an `ATMS`.
--
-- TO BE TRANSLATED from @print-nogoods@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-nogoods (atms &optional (stream t))
-- >   (print-env-table (atms-nogood-table atms) stream))
printNogoods :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
printNogoods = error "< TODO unimplemented printNogoods >"

-- |Print the `Env`ironments of an `ATMS`.
--
-- TO BE TRANSLATED from @print-envs@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-envs (atms &optional (stream t))
-- >   (print-env-table (atms-env-table atms) stream))
printEnvs :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
printEnvs = error "< TODO unimplemented printEnvs >"

-- |Print the `Env`ironments contained in the given `EnvTable`.
--
-- TO BE TRANSLATED from @print-env-table@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-env-table (table stream)
-- >   (dolist (bucket table)
-- >     (dolist (env (cdr bucket))
-- >       (print-env env stream))))
printEnvTable :: (MonadIO m, NodeDatum d) => EnvTable d i r s m -> ATMST s m ()
printEnvTable = error "< TODO unimplemented printEnvTable >"

-- |Print statistics about an `ATMS`.
--
-- TO BE TRANSLATED from @print-atms-statistics@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-atms-statistics (atms)
-- >   (print-table "~% For env table:" (atms-env-table atms))
-- >   (print-table "~% For nogood table:" (atms-nogood-table atms)))
printAtmsStatistics :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
printAtmsStatistics = error "< TODO unimplemented printAtmsStatistics >"

-- |Print the entries of an `EnvTable`.
--
-- TO BE TRANSLATED from @print-table@ in @atms.lisp@.
--
-- > ;; In atms.lisp
-- > (defun print-table (msg table)
-- >   (format t msg)
-- >   (dolist (entry table)
-- >     (format t "~%   Length ~D, ~D" (car entry)
-- >        (length (cdr entry)))))
printTable :: (MonadIO m, NodeDatum d) => String -> EnvTable d i r s m -> ATMST s m ()
printTable = error "< TODO unimplemented printTable >"

-- |Give a verbose printout of an `ATMS`.
--
debugAtms :: (MonadIO m, NodeDatum d) => String -> ATMS d i r s m -> ATMST s m ()
debugAtms blurb atms = do
  liftIO $ putStrLn $ "=============== " ++ atmsTitle atms ++ ": " ++ blurb
  debugNodes atms
  debugJusts atms
  debugAtmsEnvs atms
  debugNogoods atms
  liftIO $ putStrLn "=============== "

-- |Give a verbose printout of the `Node`s of an `ATMS`.
--
debugNodes :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
debugNodes atms = do
  nodes <- getNodes atms
  liftIO $ putStrLn $ show (length nodes) ++ " nodes:"
  forM_ (reverse nodes) debugNode

-- |Computation returning a one-line summary of one `Node` of an `ATMS`.
--
formatNode :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m String
formatNode node = do
  datumFmt <- getDatumString $ nodeATMS node
  return $ datumFmt (nodeDatum node)

-- |Computation returning a one-line summary of the `Node`s of an
-- `ATMS`.
--
formatNodes ::
  (Monad m, NodeDatum d) => String -> [Node d i r s m] -> ATMST s m String
formatNodes sep = formatList sep formatNode

-- |Computation returning a one-line summary of a list of lists of
-- `Node`s of an `ATMS`.
--
formatNodeLists ::
  (Monad m, NodeDatum d) => String -> [[Node d i r s m]] -> ATMST s m String
formatNodeLists sep = formatList sep $ formatNodes ","

-- |Computation returning a one-line summary of the label of a `Node`
-- of an `ATMS`.
--
formatNodeLabel :: (Monad m, NodeDatum d) => Node d i r s m -> ATMST s m String
formatNodeLabel node = do
  label <- getNodeLabel node
  case label of
    [] -> return "empty"
    _ -> formatNodeLists ", " $ map envAssumptions label

-- |Print a short summary of a `Node` of an `ATMS`.
--
blurbNode :: (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m ()
blurbNode node = formatNode node >>= liftIO . putStr

-- |Print a verbose summary of a `Node` of an `ATMS`.
--
-- Translated from @print-tms-node@ in @atms.lisp@.
printNode :: (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m ()
printNode node = do
  str <- nodeString node
  liftIO $ putStr $ "<NODE: " ++ str ++ ">"

-- |Give a verbose printout of a `Node` of an `ATMS`.
--
debugNode :: (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m ()
debugNode node = do
  let atms = nodeATMS node
  datumFmt <- getDatumString atms
  informantFmt <- getInformantString atms
  liftIO $ putStrLn $ "- " ++ datumFmt (nodeDatum node)

  label <- getNodeLabel node
  case label of
    [] -> liftIO $ putStrLn "  Empty label"
    [env] -> do
      liftIO $ putStr "  Single environment label: "
      debugEnv env
    _ -> forM_ label $ \env -> do
      liftIO $ putStrLn "  - "
      debugEnv env

  conseqs <- getNodeConsequences node
  case conseqs of
    [] -> liftIO $ putStrLn "  Antecedent to no justifications"
    _ -> do
      liftIO $ putStr "  Antecedent to:"
      forM_ conseqs $ \ conseq -> do
        liftIO $ putStr $ " " ++ informantFmt (justInformant conseq)
      liftIO $ putStrLn ""

-- |Computation returning a one-line summary of the reason an `ATMS`
-- may believe a `Node`.
--
formatJustification ::
  (Monad m, NodeDatum d) => Justification d i r s m -> ATMST s m String
formatJustification (ByRule j) = return $ "By rule " ++ show (justIndex j)
formatJustification (ByAssumption n) = do
  nodeFmt <- getNodeString (nodeATMS n)
  return $ "By assumption " ++ nodeFmt n
formatJustification ByContradiction = return "By contradiction"


-- |Give a verbose printout of the `Just`ification rules of an
-- `ATMS`.
--
debugJusts :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
debugJusts atms = do
  justs <- getJusts atms
  let len = length justs
  liftIO $ putStrLn $ show len ++ " justification structure"
    ++ (if len == 1 then "" else "s") ++ ":"
  forM_ (sortOn justIndex justs) $ debugJust

-- |Computation returning a one-line summary of the informant of a
-- `Just`ification rule of an `ATMS`.
--
formatJustInformant ::
  (Monad m, NodeDatum d) => JustRule d i r s m -> ATMST s m String
formatJustInformant rule = do
  informantFmt <- getInformantString $ nodeATMS $ justConsequence rule
  return $ informantFmt $ justInformant rule

-- |Print a more verbose description of a `Just`ification rule of an
-- `ATMS`.
--
-- Translated from @print-just@ in @atms.lisp@.
printJust :: (MonadIO m, NodeDatum d) => JustRule d i r s m -> ATMST s m ()
printJust rule = do
  infStr <- formatJustInformant rule
  liftIO $ putStr $ "<" ++ infStr ++ " " ++ show (justIndex rule) ++ ">"

-- |Print a more verbose description of the `Justification`.
printJustification ::
  (MonadIO m, NodeDatum d) => Justification d i r s m -> ATMST s m ()
printJustification j = case j of
  ByRule rule -> printJust rule
  ByAssumption node -> do
    liftIO $ putStr $ "Assumed node "
    printNode node
  ByContradiction -> liftIO $ putStrLn $ "By contradiction"

-- |Give a verbose printout of one `Just`ification rule of an `ATMS`.
--
debugJust :: (MonadIO m, NodeDatum d) => JustRule d i r s m -> ATMST s m ()
debugJust (JustRule idx inf conseq ants) = do
  let atms = nodeATMS conseq
  informantFmt <- getInformantString atms
  datumFmt <- getDatumString atms
  liftIO $ putStrLn $ "  "
    ++ "[" ++ informantFmt inf ++ "." ++ show idx ++ "] "
    ++ datumFmt (nodeDatum conseq) ++ " <= "
    ++ intercalate ", " (map (datumFmt . nodeDatum) ants)

-- |Give a verbose printout of the `Env`ironments of an `ATMS`.
--
debugAtmsEnvs :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
debugAtmsEnvs atms = do
  liftIO $ putStrLn "Environments:"
  envTable <- getEnvTable atms
  debugEnvTable atms envTable

-- |Give a verbose printout of one `Env`ironment of an `ATMS`.
--
debugEnv :: (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
debugEnv env = do
  isNogood <- envIsNogood env
  case envAssumptions env of
    [] -> liftIO $ putStrLn "<empty>"
    nodes @ (n : _) -> do
      let atms = nodeATMS n
      datumFmt <- getDatumString atms
      when isNogood $ liftIO $ putStr "[X] "
      liftIO $ putStrLn $
        (intercalate ", " $ map (datumFmt . nodeDatum) nodes)
        ++ " (count " ++ show (length nodes) ++ ")"

-- |Print a short summary of a mutable list of nullable (via `Maybe`)
-- `Env`ironments from an `ATMS`.
--
blurbMaybeEnvMList ::
  (MonadIO m, NodeDatum d) => MList s (Maybe (Env d i r s m)) -> ATMST s m ()
blurbMaybeEnvMList mlist = do
  sep <- sttLayer $ newSTRef ""
  liftIO $ putStr "m["
  mlistFor_ sttLayer mlist $ \envm -> do
    thisSep <- sttLayer $ readSTRef sep
    liftIO $ putStr thisSep
    case envm of
      Just env -> blurbEnv env
      Nothing -> liftIO $ putStr "<nothing>"
    sttLayer $ writeSTRef sep ", "
  liftIO $ putStr "]"

-- |Print a short summary of a reference to a mutable list of
-- nullable (via `Maybe`) `Env`ironments from an `ATMS`.
--
blurbMaybeEnvMListRef ::
  (MonadIO m, NodeDatum d) =>
    STRef s (MList s (Maybe (Env d i r s m))) -> ATMST s m ()
blurbMaybeEnvMListRef mlistRef = do
  mlist <- sttLayer $ readSTRef mlistRef
  blurbMaybeEnvMList mlist

-- |Print a short summary of a nullable (via `Maybe`) reference to an
-- `Env`ironment of an `ATMS`.
--
blurbMaybeEnv ::
  (MonadIO m, NodeDatum d) => Maybe (Env d i r s m) -> ATMST s m ()
blurbMaybeEnv envm = case envm of
                       Just env -> blurbEnv env
                       Nothing -> liftIO $ putStr "<nothing>"

-- |Print a short summary of one `Env`ironment of an `ATMS`.
--
blurbEnv :: (MonadIO m, NodeDatum d) => Env d i r s m -> ATMST s m ()
blurbEnv env = do
  wng <- sttLayer $ readSTRef $ envWhyNogood env
  isNogood <- envIsNogood env
  case envAssumptions env of
    [] -> do
      liftIO $ putStr "<empty>"
    nodes @ (first : _) -> do
      datumFmt <- getDatumString (nodeATMS first)
      when isNogood $ liftIO $ putStr "[X] "
      liftIO $ putStr $
        "{" ++ (intercalate ", " $ map (datumFmt . nodeDatum) nodes) ++ "}"

-- |Give a verbose printout of the no-good `Env`ironments of an
-- `ATMS`.
--
debugNogoods :: (MonadIO m, NodeDatum d) => ATMS d i r s m -> ATMST s m ()
debugNogoods atms = do
  liftIO $ putStrLn "No-good environments:"
  nogoodTable <- getNogoodTable atms
  debugEnvTable atms nogoodTable

-- |Give a verbose printout of the `Env`ironments of an `EnvTable` of
-- an `ATMS`.
--
debugEnvTable ::
  (MonadIO m, NodeDatum d) =>
    ATMS d i r s m -> EnvTable d i r s m -> ATMST s m ()
debugEnvTable atms (EnvTable array) = do
  let (lo, hi) = boundsSTArray array
  forM_ [lo..hi] $ \ i -> do
    envs <- sttLayer $ readSTArray array i
    forM_ (reverse envs) $ \ env -> do
      liftIO $ putStr "- "
      debugEnv env

{-
-- |Print a short summary of the label of a `Node` of an `ATMS`.
--
blurbNodeLabel ::
  (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m String
blurbNodeLabel node = do
  -- lbl <- getNodeLabel node
  lbl <- sttLayer $ readSTRef (nodeLabel node)
  blurbNode node
  liftIO $ putStr " label: "
  blurbEnvList 10000 "\n" lbl
  liftIO $ putStrLn ""
-}

-- |Give a verbose printout of the label of a `Node` of an `ATMS`.
--
debugNodeLabel ::
  (MonadIO m, NodeDatum d) => Node d i r s m -> ATMST s m ()
debugNodeLabel node = do
  -- lbl <- getNodeLabel node
  lbl <- sttLayer $ readSTRef (nodeLabel node)
  blurbNode node
  liftIO $ putStr " label: "
  blurbEnvList 10000 "\n" lbl
  liftIO $ putStrLn ""

-- |Print a short summary of a list of `Env`ironments of an `ATMS`.
--
blurbEnvList ::
  (MonadIO m, NodeDatum d) => Int -> String -> [Env d i r s m] -> ATMST s m ()
blurbEnvList multiLineIf lineLead envs =
  case length envs of
    0 -> liftIO $ putStr "empty env list"
    n | n < multiLineIf -> do
          liftIO $ putStr $ show n ++" envs: "
          sepR <- sttLayer $ newSTRef ""
          forM_ envs $ \env -> do
            sep <- sttLayer $ readSTRef sepR
            liftIO $ putStr sep
            blurbEnv env
            sttLayer $ writeSTRef sepR ", "
    n -> do
      liftIO $ putStrLn $ show n ++" envs:"
      forM_ envs $ \env -> do
        liftIO $ putStr lineLead
        blurbEnv env
        liftIO $ putStrLn ""

instance MonadIO m => MonadIO (STT s m) where liftIO = lift . liftIO
