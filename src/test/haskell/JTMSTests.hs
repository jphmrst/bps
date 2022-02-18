{-|
Description : Testing Justification-based truth maintenance systems (JTMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Translation of Forbus and de Kleer's justification-based truth
maintenance systems (JTMSes) from Common Lisp to Haskell.

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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module JTMSTests (testEx1, testEx3) where

import Data.Symbol
import Data.Void
import Data.TMS.JTMS
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Test.TLT

-- Prints result of the string length calculation.
report :: Either JtmsErr () -> IO ()
report (Right _) = putStrLn ("Tests passed")
report (Left e) = putStrLn ("Caught exception: " ++ (show e))

instance MonadTLT m n => MonadTLT (JTMST s m) n where
  liftTLT = lift . liftTLT

type JTMS1ty s m = JTMS Symbol String Void s m
type Node1ty s m = Node Symbol String Void s m
ex1 :: Monad m => JTMST s m (JTMS1ty s m,
                             Node1ty s m, Node1ty s m, Node1ty s m,
                             Node1ty s m, Node1ty s m, Node1ty s m,
                             Node1ty s m)
ex1 = do
  j <- createJTMS "Ex1"
  nodeStringByDatum j
  datumStringByShow j
  informantStringByShow j
  justStringByIndexInformant j
  setNodeString j (show . nodeDatum)
  na <- createNode j (intern "a") True False
  nb <- createNode j (intern "b") True False
  nc <- createNode j (intern "c") True False
  nd <- createNode j (intern "d") True False
  ne <- createNode j (intern "e") True False
  nf <- createNode j (intern "f") True False
  ng <- createNode j (intern "g") True False
  justifyNode "j1" nf [na, nb]
  justifyNode "j2" ne [nb, nc]
  justifyNode "j3" ng [na, ne]
  justifyNode "j4" ng [nd, ne]
  return (j, na, nb, nc, nd, ne, nf, ng)

testEx1 :: MonadIO m => JTMST s (TLT m) ()
testEx1 = do
  (jtms, na, nb, nc, nd, ne, nf, ng) <- ex1
  datumStringByShow jtms
  inGroup "Fresh JTMS" $ do
    assertBeliefs jtms [] [na, nb, nc, nd, ne, nf, ng]
    assertNoAssumptionsOfNodes jtms [na, nb, nc, nd, ne, nf, ng]
    assertNodesUnsupported jtms [na, nb, nc, nd, ne, nf, ng]

  -- debugJTMS "fresh" jtms
  enableAssumption na
  -- debugJTMS "after (enableAssumption na)" jtms
  inGroup "Enabled a as assumption" $ do
    assertBeliefs jtms [na] [nb, nc, nd, ne, nf, ng]
    assertAssumptionsOfNode jtms na [na]
    assertNoAssumptionsOfNodes jtms [nb, nc, nd, ne, nf, ng]
    assertNodeSupportEnabledAssumption jtms na
    assertNodesUnsupported jtms [nb, nc, nd, ne, nf, ng]

  enableAssumption nb
  -- debugJTMS "after (enableAssumption nb)" jtms
  inGroup "Enabled b as assumption" $ do
    assertBeliefs jtms [na, nb, nf] [nc, nd, ne, ng]
    assertAssumptionsOfNode jtms na [na]
    assertAssumptionsOfNode jtms nb [nb]
    assertAssumptionsOfNode jtms nf [na, nb]
    assertNoAssumptionsOfNodes jtms [nc, nd, ne, ng]
    assertNodesSupportEnabledAssumption jtms [na, nb]
    assertNodeSupportInformant jtms nf "j1"
    assertNodesUnsupported jtms [nc, nd, ne, ng]

  enableAssumption nc
  -- debugJTMS "after (enableAssumption nc)" jtms
  inGroup "Enabled c as assumption" $ do
    assertBeliefs jtms [na, nb, nc, ne, nf, ng] [nd]
    assertAssumptionsOfNode jtms na [na]
    assertAssumptionsOfNode jtms nb [nb]
    assertAssumptionsOfNode jtms nc [nc]
    assertAssumptionsOfNode jtms ne [nb, nc]
    assertAssumptionsOfNode jtms nf [na, nb]
    assertAssumptionsOfNode jtms ng [na, nb, nc]
    assertNoAssumptionsOfNodes jtms [nd]
    assertNodesSupportEnabledAssumption jtms [na, nb, nc]
    assertNodeSupportInformant jtms ne "j2"
    assertNodeSupportInformant jtms nf "j1"
    assertNodeSupportInformant jtms ng "j3"
    assertNodesUnsupported jtms [nd]

  enableAssumption nd
  -- debugJTMS "after (enableAssumption nd)" jtms
  inGroup "Enabled d as assumption" $ do
    assertBeliefs jtms [na, nb, nc, nd, ne, nf, ng] []
    assertAssumptionsOfNode jtms na [na]
    assertAssumptionsOfNode jtms nb [nb]
    assertAssumptionsOfNode jtms nc [nc]
    assertAssumptionsOfNode jtms nd [nd]
    assertAssumptionsOfNode jtms ne [nb, nc]
    assertAssumptionsOfNode jtms nf [na, nb]
    assertAssumptionsOfNode jtms ng [na, nb, nc]
    assertNodesSupportEnabledAssumption jtms [na, nb, nc, nd]
    assertNodeSupportInformant jtms ne "j2"
    assertNodeSupportInformant jtms nf "j1"
    assertNodeSupportInformant jtms ng "j3"

  retractAssumption na
  -- debugJTMS "after (retractAssumption na)" jtms
  inGroup "Retracted a as assumption" $ do
    assertBeliefs jtms [nb, nc, nd, ne, ng] [na, nf]
    assertAssumptionsOfNode jtms nb [nb]
    assertAssumptionsOfNode jtms nc [nc]
    assertAssumptionsOfNode jtms nd [nd]
    assertAssumptionsOfNode jtms ne [nb, nc]
    assertAssumptionsOfNode jtms ng [nb, nc, nd]
    assertNoAssumptionsOfNodes jtms [na, nf]
    assertNodesUnsupported jtms [na, nf]
    assertNodesSupportEnabledAssumption jtms [nb, nc, nd]
    assertNodeSupportInformant jtms ne "j2"
    assertNodeSupportInformant jtms ng "j4"

type JTMS3ty s m = JTMS Symbol String () s m
type Node3ty s m = Node Symbol String () s m
ex3 :: Monad m => JTMST s m (JTMS3ty s m,
                             Node3ty s m, Node3ty s m, Node3ty s m,
                             Node3ty s m, Node3ty s m, Node3ty s m)
ex3 = do
  j <- createJTMS "Ex1"

  na <- createNode j (intern "A") True  False
  nc <- createNode j (intern "C") True  False
  ne <- createNode j (intern "E") True  False
  ng <- createNode j (intern "g") False False
  nh <- createNode j (intern "h") False False
  contra <- createNode j (intern "CONTRADICTION") False True

  justifyNode "R1" nh [nc, ne]
  justifyNode "R2" ng [na, nc]
  justifyNode "R3" contra [ng]

  return (j, na, nc, ne, ng, nh, contra)

testEx3 :: MonadIO m => JTMST s (TLT (STT s0 m)) ()
testEx3 = do
  contraHandlerFlag <- lift $ lift $ newSTRef False

  (jtms, na, nc, ne, ng, nh, contra) <- ex3
  setContradictionHandler jtms $ \_ ->
    lift $ lift $ writeSTRef contraHandlerFlag True
  datumStringByShow jtms
  inGroup "Fresh JTMS" $ do
    assertBeliefs jtms [] [na, nc, ne, ng, nh, contra]
    assertNoAssumptionsOfNodes jtms [na, nc, ne, ng, nh, contra]
    assertNodesUnsupported jtms [na, nc, ne, ng, nh, contra]
    "No call to contradiction handler" ~::
      notM (lift $ lift $ readSTRef contraHandlerFlag)

  lift $ lift $ writeSTRef contraHandlerFlag False
  enableAssumption na
  inGroup "Enabled a" $ do
    assertBeliefs jtms [na] [nc, ne, ng, nh, contra]
    assertAssumptionsOfNode jtms na [na]
    assertNoAssumptionsOfNodes jtms [nc, ne, ng, nh, contra]
    "No call to contradiction handler" ~::
      notM (lift $ lift $ readSTRef contraHandlerFlag)

  lift $ lift $ writeSTRef contraHandlerFlag False
  enableAssumption nc
  inGroup "Enabled c" $ do
    assertBeliefs jtms [na, nc, ng, contra] [ne, nh]
    "Did call contradiction handler" ~::
      (lift $ lift $ readSTRef contraHandlerFlag)

  lift $ lift $ writeSTRef contraHandlerFlag False
  enableAssumption ne
  inGroup "Enabled e" $ do
    assertBeliefs jtms [na, nc, ne, ng, nh, contra] []
    "Did call contradiction handler" ~::
      (lift $ lift $ readSTRef contraHandlerFlag)

{------------------------- Local assertions. -------------------------}

assertBeliefs ::
  Monad m => (JTMS d i r s (TLT m)) -> [Node d i r s (TLT m)] -> [Node d i r s (TLT m)] ->
               JTMST s (TLT m) ()
assertBeliefs jtms ins outs = inGroup "Node belief" $ do
  inChecks
  outChecks
  where inChecks = forM_ ins inCheck

        inCheck node = do
          name <- nodeString node
          ("Node " ++ name ++ " is in") ~:: isInNode node

        outChecks = forM_ outs $ outCheck

        outCheck node = do
          name <- nodeString node
          ("Node " ++ name ++ " is out") ~:: isOutNode node

assertAssumptionsOfNode ::
  Monad m =>
    (JTMS d i r s (TLT m)) -> Node d i r s (TLT m) -> [Node d i r s (TLT m)] ->
      JTMST s (TLT m) ()
assertAssumptionsOfNode jtms node assumptions = do
  actuals <- assumptionsOfNode node
  name <- nodeString node
  inGroup ("Checking assumptionsOfNode " ++ name) $ do
    ("Same number of expected and actual assumptions")
      ~: length assumptions !==- length actuals
    forM_ assumptions $ \ expected -> do
      expName <- nodeString expected
      ("Contains expected node " ++ expName) ~::- (expected `elem` actuals)

assertNoAssumptionsOfNodes ::
  Monad m =>
    (JTMS d i r s (TLT m)) -> [Node d i r s (TLT m)] -> JTMST s (TLT m) ()
assertNoAssumptionsOfNodes jtms nodes =
  inGroup ("No assumptionsOfNode") $
    forM_ nodes $ \ node -> do
      name <- nodeString node
      ("Node " ++ name ++ " has no assumptions") ~:
        (empty $ assumptionsOfNode node)

assertNodesUnsupported ::
  Monad m =>
    (JTMS d i r s (TLT m)) -> [Node d i r s (TLT m)] -> JTMST s (TLT m) ()
assertNodesUnsupported jtms nodes =
  inGroup ("Unsupported nodes") $
    forM_ nodes $ \ node -> do
      name <- nodeString node
      ("Node " ++ name ++ " has no support") ~::
        do support <- getNodeSupport node
           case support of
             Nothing -> return True
             Just _ -> return False

assertNodeSupportRule ::
  Monad m =>
    JTMS d i r s (TLT m) -> Node d i r s (TLT m) -> JustRule d i r s (TLT m) ->
      JTMST s (TLT m) ()
assertNodeSupportRule jtms node just = do
  infString <- getJtmsInformantString jtms
  nodeName <- nodeString node
  ("Node " ++ nodeName ++ " supported by rule "
    ++ (infString $ justInformant just)) ~:: do
    support <- getNodeSupport node
    case support of
      Just (ByRule j) | j == just -> return True
      _ -> return False

assertNodeSupportInformant ::
  (Monad m, Eq i) =>
    JTMS d i r s (TLT m) -> Node d i r s (TLT m) -> i -> JTMST s (TLT m) ()
assertNodeSupportInformant jtms node inf = do
  infString <- getJtmsInformantString jtms
  nodeName <- nodeString node
  ("Node " ++ nodeName ++ " supported by informant " ++ (infString inf)) ~:: do
    support <- getNodeSupport node
    case support of
      Just (ByRule j) | (justInformant j) == inf -> return True
      _ -> return False

assertNodeSupportEnabledAssumption ::
  Monad m => JTMS d i r s (TLT m) -> Node d i r s (TLT m) -> JTMST s (TLT m) ()
assertNodeSupportEnabledAssumption jtms node = do
  infString <- getJtmsInformantString jtms
  nodeName <- nodeString node
  ("Node " ++ nodeName ++ " is enabled assumption ") ~:: do
    support <- getNodeSupport node
    case support of
      Just EnabledAssumption -> return True
      _ -> return False

assertNodesSupportEnabledAssumption ::
  Monad m =>
    JTMS d i r s (TLT m) -> [Node d i r s (TLT m)] -> JTMST s (TLT m) ()
assertNodesSupportEnabledAssumption jtms nodes =
  inGroup ("Nodes are enabled assumptions") $
    forM_ nodes $ \ node -> assertNodeSupportEnabledAssumption jtms node

assertNodeSupportUserStipulation ::
  Monad m => JTMS d i r s (TLT m) -> Node d i r s (TLT m) -> JTMST s (TLT m) ()
assertNodeSupportUserStipulation jtms node = do
  infString <- getJtmsInformantString jtms
  nodeName <- nodeString node
  ("Node " ++ nodeName ++ " is enabled assumption ") ~:: do
    support <- getNodeSupport node
    case support of
      Just UserStipulation -> return True
      _ -> return False
