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
import Testers

-- type LTMS1ty s m = LTMS String String Void s m
-- type Node1ty s m = Node String String Void s m
ltmsTest1 :: MonadIO m => LTMST s (TLT m) ()
ltmsTest1 = inGroup "LTMS Test 1" $ do
  ltms <- createLTMS "Ex1"
  setInformantStringViaString ltms
  setDatumStringViaString ltms

{-
  inGroup "Freshly created LTMS" $ do
    assertAssumptionsAre ltms []
    assertContradictionsAre ltms []

  na <- createNode ltms "A" True False
  inGroup "Created Node A" $ do
    assertSingleSelfLabel na
    assertAssumptionsAre ltms [na]
    assertContradictionsAre ltms []
-}
