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

import Data.Symbol
import Data.Void
import Data.TMS.JTMS
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Test.TLT

main :: IO ()
main = do
  runJTMST $ tlt $ do
    testEx1
  return ()

-- Prints result of the string length calculation.
report :: Either JtmsErr () -> IO ()
report (Right _) = putStrLn ("Tests passed")
report (Left e) = putStrLn ("Caught exception: " ++ (show e))

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

testEx1 :: MonadIO m => TLT (JTMST s m) ()
testEx1 = do
  (jtms, na, nb, nc, nd, ne, nf, ng) <- lift ex1
  lift $ datumStringByShow jtms
  inGroup "Fresh JTMS" $ do
    assertBeliefs jtms [] [na, nb, nc, nd, ne, nf, ng]
    assertNoAssumptionsOfNodes jtms [na, nb, nc, nd, ne, nf, ng]
    assertNodesUnsupported jtms [na, nb, nc, nd, ne, nf, ng]

  -- lift $ debugJTMS "fresh" jtms
  lift $ enableAssumption na
  -- lift $ debugJTMS "after (enableAssumption na)" jtms
  inGroup "Enabled a as assumption" $ do
    assertBeliefs jtms [na] [nb, nc, nd, ne, nf, ng]
    assertAssumptionsOfNode jtms na [na]
    assertNoAssumptionsOfNodes jtms [nb, nc, nd, ne, nf, ng]
    assertNodeSupportEnabledAssumption jtms na
    assertNodesUnsupported jtms [nb, nc, nd, ne, nf, ng]

  lift $ enableAssumption nb
  -- lift $ debugJTMS "after (enableAssumption nb)" jtms
  inGroup "Enabled b as assumption" $ do
    assertBeliefs jtms [na, nb, nf] [nc, nd, ne, ng]
    assertAssumptionsOfNode jtms na [na]
    assertAssumptionsOfNode jtms nb [nb]
    assertAssumptionsOfNode jtms nf [na, nb]
    assertNoAssumptionsOfNodes jtms [nc, nd, ne, ng]
    assertNodesSupportEnabledAssumption jtms [na, nb]
    assertNodeSupportInformant jtms nf "j1"
    assertNodesUnsupported jtms [nc, nd, ne, ng]

  lift $ enableAssumption nc
  -- lift $ debugJTMS "after (enableAssumption nc)" jtms
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

  lift $ enableAssumption nd
  -- lift $ debugJTMS "after (enableAssumption nd)" jtms
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

{- Local assertions. -}

assertBeliefs ::
  Monad m => (JTMS d i r s m) -> [Node d i r s m] -> [Node d i r s m] ->
               TLT (JTMST s m) ()
assertBeliefs jtms ins outs = inGroup "Node belief" $ do
  forM_ ins  $ \ node -> do
    name <- lift $ nodeString node
    ("Node " ++ name ++ " is in") ~:: isInNode node
  forM_ outs $ \ node -> do
    name <- lift $ nodeString node
    ("Node " ++ name ++ " is out") ~:: isOutNode node

assertAssumptionsOfNode ::
  Monad m => (JTMS d i r s m) -> Node d i r s m -> [Node d i r s m] ->
               TLT (JTMST s m) ()
assertAssumptionsOfNode jtms node assumptions = do
  actuals <- lift $ assumptionsOfNode node
  name <- lift $ nodeString node
  inGroup ("Checking assumptionsOfNode " ++ name) $ do
    ("Same number of expected and actual assumptions")
      ~: length assumptions !==- length actuals
    forM_ assumptions $ \ expected -> do
      expName <- lift $ nodeString expected
      ("Contains expected node " ++ expName) ~::- (expected `elem` actuals)

assertNoAssumptionsOfNodes ::
  Monad m => (JTMS d i r s m) -> [Node d i r s m] -> TLT (JTMST s m) ()
assertNoAssumptionsOfNodes jtms nodes =
  inGroup ("No assumptionsOfNode") $
    forM_ nodes $ \ node -> do
      name <- lift $ nodeString node
      ("Node " ++ name ++ " has no assumptions") ~:
        (empty $ assumptionsOfNode node)

assertNodesUnsupported ::
  Monad m => (JTMS d i r s m) -> [Node d i r s m] -> TLT (JTMST s m) ()
assertNodesUnsupported jtms nodes =
  inGroup ("Unsupported nodes") $
    forM_ nodes $ \ node -> do
      name <- lift $ nodeString node
      ("Node " ++ name ++ " has no support") ~::
        do support <- getNodeSupport node
           case support of
             Nothing -> return True
             Just _ -> return False

assertNodeSupportRule :: Monad m =>
  JTMS d i r s m -> Node d i r s m -> JustRule d i r s m -> TLT (JTMST s m) ()
assertNodeSupportRule jtms node just = do
  infString <- lift $ getJtmsInformantString jtms
  nodeName <- lift $ nodeString node
  ("Node " ++ nodeName ++ " supported by rule "
    ++ (infString $ justInformant just)) ~:: do
    support <- getNodeSupport node
    case support of
      Just (ByRule j) | j == just -> return True
      _ -> return False

assertNodeSupportInformant :: (Monad m, Eq i) =>
  JTMS d i r s m -> Node d i r s m -> i -> TLT (JTMST s m) ()
assertNodeSupportInformant jtms node inf = do
  infString <- lift $ getJtmsInformantString jtms
  nodeName <- lift $ nodeString node
  ("Node " ++ nodeName ++ " supported by informant " ++ (infString inf)) ~:: do
    support <- getNodeSupport node
    case support of
      Just (ByRule j) | (justInformant j) == inf -> return True
      _ -> return False

assertNodeSupportEnabledAssumption ::
  Monad m => JTMS d i r s m -> Node d i r s m -> TLT (JTMST s m) ()
assertNodeSupportEnabledAssumption jtms node = do
  infString <- lift $ getJtmsInformantString jtms
  nodeName <- lift $ nodeString node
  ("Node " ++ nodeName ++ " is enabled assumption ") ~:: do
    support <- getNodeSupport node
    case support of
      Just EnabledAssumption -> return True
      _ -> return False

assertNodesSupportEnabledAssumption ::
  Monad m => JTMS d i r s m -> [Node d i r s m] -> TLT (JTMST s m) ()
assertNodesSupportEnabledAssumption jtms nodes =
  inGroup ("Nodes are enabled assumptions") $
    forM_ nodes $ \ node -> assertNodeSupportEnabledAssumption jtms node

assertNodeSupportUserStipulation ::
  Monad m => JTMS d i r s m -> Node d i r s m -> TLT (JTMST s m) ()
assertNodeSupportUserStipulation jtms node = do
  infString <- lift $ getJtmsInformantString jtms
  nodeName <- lift $ nodeString node
  ("Node " ++ nodeName ++ " is enabled assumption ") ~:: do
    support <- getNodeSupport node
    case support of
      Just UserStipulation -> return True
      _ -> return False
