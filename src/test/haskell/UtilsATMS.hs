{-|
Description : Testing assumption-based truth maintenance systems (ATMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Testing the Haskell translation of Forbus and de Kleer's
assumption-based truth maintenance systems (JTMSes).

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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module UtilsATMS where

import Data.List
import Data.Symbol
import Data.Void
import Data.TMS.ATMS.ATMST
import Data.TMS.Helpers
import Data.TMS.Formatters
import Data.TMS.Common
import Data.TMS.Dbg
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Test.TLT

-- Prints result of the string length calculation.
report :: Either AtmsErr () -> IO ()
report (Right _) = putStrLn ("Tests passed")
report (Left e) = putStrLn ("Caught exception: " ++ (show e))

instance MonadTLT m n => MonadTLT (ATMST s m) n where
  liftTLT = lift . liftTLT

assertTrueNode ::
  (MonadIO m, NodeDatum d) => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertTrueNode node = "Node is true" ~:: isTrueNode node

assertInNode ::
  (MonadIO m, NodeDatum d) => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertInNode node = "Node is in" ~:: isInNode node

assertNotTrueNode ::
  (MonadIO m, NodeDatum d) => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertNotTrueNode node = "Node is not true" ~:: (fmap not $ isTrueNode node)

assertNoLabel ::
  (MonadIO m, NodeDatum d) => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertNoLabel node = do
  labels <- getNodeLabel node
  "No labels" ~: 0 @==- length labels

assertSingleSelfLabels ::
  (MonadIO m, NodeDatum d) => [Node d i r s (TLT m)] -> ATMST s (TLT m) ()
assertSingleSelfLabels nodes =
  forM_ nodes $ \ node -> assertSingleSelfLabel node

assertSingleSelfLabel ::
  (MonadIO m, NodeDatum d) => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertSingleSelfLabel node = assertSingleLabelEnvBy node [node]

assertSingleLabelEnvBy ::
  (MonadIO m, NodeDatum d) =>
    Node d i r s (TLT m) -> [Node d i r s (TLT m)] -> ATMST s (TLT m) ()
assertSingleLabelEnvBy node nodes =
  inGroup (show node ++ " labelled by one Env with "
            ++ intercalate ", " (map show nodes)) $ do
    assertNotTrueNode node
    assertInNode node
    labels <- getNodeLabel node
    case labels of
      [env] -> do
        let envAsmpts = envAssumptions env
        "Single label should have " ++ show (length nodes) ++ " assumptions" ~:
          length nodes @==- length envAsmpts
        forM_ nodes $ \ node -> do
          "Label should contain " ++ show node ~::- elem node envAsmpts
      l -> "Expected one Env in label" `tltFail` ("Found " ++ (show $ length l))

assertNodeLabelAssumptions ::
  (MonadIO m, NodeDatum d) =>
    Node d i r s (TLT m) -> [[Node d i r s (TLT m)]] -> ATMST s (TLT m) ()
assertNodeLabelAssumptions node nodeLists = do
  let atms = nodeATMS node
  datumFmt <- getDatumString atms
  showLabel <- formatNodeLabel node
  labelNodeLists <- fmap (fmap envAssumptions) $ getNodeLabel node
  inGroup ("Checking node " ++ (datumFmt $ nodeDatum node)
           ++ " label " ++ showLabel) $ do
    "Expect " ++ show (length nodeLists) ++ " environments" ~:
      length nodeLists @==- length labelNodeLists
    forM_ nodeLists $ \ nodeList -> do
      nl <- formats "(no nodes)" "," nodeList
      "Should have environment with assumptions " ++ nl
        ~::- elem nodeList labelNodeLists

assertAssumptionsAre ::
  (MonadIO m, NodeDatum d) =>
    ATMS d i r s (TLT m) -> [Node d i r s (TLT m)] -> ATMST s (TLT m) ()
assertAssumptionsAre = assertAtmsNodeGroup "assumptions" getAssumptions

assertContradictionsAre ::
  (MonadIO m, NodeDatum d) =>
    ATMS d i r s (TLT m) -> [Node d i r s (TLT m)] -> ATMST s (TLT m) ()
assertContradictionsAre atms nodes = do
  builtIn <- getContradictionNode atms
  assertAtmsNodeGroup "contradictions" getContradictions atms (builtIn : nodes)

assertAtmsNodeGroup ::
  (MonadIO m, NodeDatum d) =>
    String ->
      (ATMS d i r s (TLT m) -> ATMST s (TLT m) [Node d i r s (TLT m)]) ->
        ATMS d i r s (TLT m) -> [Node d i r s (TLT m)] ->
          ATMST s (TLT m) ()
assertAtmsNodeGroup title extractor atms nodes =
  inGroup ("Checking " ++ title ++ " in ATMS") $ do
    assumptionsList <- extractor atms
    "Should have " ++ (show $ length nodes) ++ " " ++ title ~:
      length nodes @==- length assumptionsList
    forM_ nodes $ \ node -> do
      show node ++ " should be present" ~::- elem node assumptionsList
