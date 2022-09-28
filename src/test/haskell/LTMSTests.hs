{-|
Description : Testing assumption-based truth maintenance systems (LTMSes)
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

module LTMSTests where

import Data.List
import Data.Symbol
import Data.Void
import Data.TMS.LTMS
import Data.TMS.Helpers
import Data.TMS.Dbg
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Test.TLT

instance MonadTLT m n => MonadTLT (LTMST s m) n where
  liftTLT = lift . liftTLT

ltmsTest0 :: MonadIO m => LTMST s (TLT m) (LTMS String String () s (TLT m))
ltmsTest0 = inGroup "LTMS Test 1" $ do
  ltms <- createLTMS "Ex1"
  setInformantStringViaString ltms
  setDatumStringViaString ltms
  return ltms

-- | From @test-explain@ in @ltms-ex.lisp@.
ltmsTestExplain ::
  MonadIO m => LTMST s (TLT m) (LTMS String String () s (TLT m))
ltmsTestExplain = inGroup "defun test-explain" $ do
  ltms <- createLTMS "test-explain"
  setInformantStringViaString ltms
  setDatumStringViaString ltms

  x <- createNode ltms "x"
  compileFormula ltms $ Or [Datum "x", Datum "y"]
  compileFormula ltms $ Or [Not (Datum "y"), Datum "z"]
  compileFormula ltms $ Or [Not (Datum "z"), Datum "r"]
  enableAssumption x labelFalse
  -- whyNodes ltms
  return ltms

-- | From @test-explain@ in @ltms-ex.lisp@.
ltmsTestAsk ::
  MonadIO m => LTMST s (TLT m) (LTMS String String () s (TLT m))
ltmsTestAsk = inGroup "defun test-ask" $ do
  ltms <- createLTMS "test-ask"
  setInformantStringViaString ltms
  setDatumStringViaString ltms
  n1 <- createNode ltms "N1"
  convertToAssumption n1
  n2 <- createNode ltms "N2"
  convertToAssumption n2
  enableAssumption n1 labelFalse
  enableAssumption n2 labelFalse
  compileFormula ltms $ Or [Datum "N1", DNode n2]
  -- whyNodes ltms
  return ltms

-- | From @test1@ in @ltms-ex.lisp@.
ltmsTest1 ::
  MonadIO m => LTMST s (TLT m) (LTMS String String () s (TLT m))
ltmsTest1 = inGroup "defun test1" $ do
  ltms <- createLTMS "test1"
  setInformantStringViaString ltms
  setDatumStringViaString ltms
  setComplete ltms True
  x <- createNode ltms "x"
  y <- createNode ltms "y"
  addFormula ltms "f1" $ Or [DNode x, DNode y]
  addFormula ltms "f2" $ Or [DNode x, Not $ DNode y]
  -- completeLtms ltms
  "Node x should be true" ~: True @== isTrueNode x
  -- TODO Assert (true-node? x)
  return ltms
