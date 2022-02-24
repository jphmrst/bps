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

module ATMSTests where

import Data.Symbol
import Data.Void
import Data.TMS.ATMS.ATMST
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

type ATMS1ty s m = ATMS String String Void s m
type Node1ty s m = Node String String Void s m
ex1AndTest :: Monad m => ATMST s (TLT m) ()
ex1AndTest = inGroup "ATMS Test 1" $ do
  atms <- createATMS "Ex1"

  na <- createNode atms "A" True False
  inGroup "Created Node A" $ do
    assertSingleSelfLabel na

  nc <- createNode atms "C" True False
  inGroup "Created Node C" $ do
    assertSingleSelfLabel nc

  ne <- createNode atms "E" True False
  inGroup "Created Node E" $ do
    assertSingleSelfLabel ne

  nh <- createNode atms "H" False False
  inGroup "Created Node H" $ do
    assertNoLabel nh

  justifyNode "R1" nh [nc, ne]
  inGroup "Created Node H" $ do
    -- TODO Add tests here
    return ()

  ng <- createNode atms "G" False False
  inGroup "Created Node G" $ do
    assertNoLabel ng

  return ()

assertNoLabel :: Monad m => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertNoLabel node = do
  labels <- getNodeLabel node
  "No labels" ~: 0 !==- length labels

assertSingleSelfLabel :: Monad m => Node d i r s (TLT m) -> ATMST s (TLT m) ()
assertSingleSelfLabel node = do
  labels <- getNodeLabel node
  "Initially 1 label" ~: 1 !==- length labels
  -- TODO Pull the env out of the labels, find only node in the node
  -- list.
  case labels of
    -- [lab] -> "Label is A" ~::- lab == na
    _ -> return ()

