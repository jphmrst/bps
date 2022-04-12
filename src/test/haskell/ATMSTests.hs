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

import Data.List
import Data.Symbol
import Data.Void
import Data.TMS.ATMS.ATMST
import Data.TMS.Helpers
import Data.TMS.Dbg
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Test.TLT
import Testers

type ATMS1ty s m = ATMS String String Void s m
type Node1ty s m = Node String String Void s m
ex1AndTest :: MonadIO m => ATMST s (TLT m) ()
ex1AndTest = inGroup "ATMS Test 1" $ do
  atms <- createATMS "Ex1"
  setInformantStringViaString atms
  setDatumStringViaString atms

  inGroup "Freshly created ATMS" $ do
    assertAssumptionsAre atms []
    assertContradictionsAre atms []

  na <- createNode atms "A" True False
  inGroup "Created Node A" $ do
    assertSingleSelfLabel na
    assertAssumptionsAre atms [na]
    assertContradictionsAre atms []

  nc <- createNode atms "C" True False
  inGroup "Created Node C" $ do
    assertSingleSelfLabels [na, nc]
    assertAssumptionsAre atms [na, nc]
    assertContradictionsAre atms []

  ne <- createNode atms "E" True False
  inGroup "Created Node E" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms []

  nh <- createNode atms "H" False False
  inGroup "Created Node H" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertNoLabel nh
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms []

  justifyNode "R1" nh [nc, ne]
  inGroup "Added Justification R1" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms []

  ng <- createNode atms "G" False False
  inGroup "Created Node G" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertNoLabel ng
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms []

  justifyNode "R2" ng [na, nc]
  inGroup "Added Justification R2" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertSingleLabelEnvBy ng [na, nc]
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms []

  nx <- createNode atms "X" False True
  inGroup "Created Node X" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertSingleLabelEnvBy ng [na, nc]
    assertNoLabel nx
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms [nx]

  justifyNode "R3" nx [ng]
  inGroup "Added Justification R3" $ do
    assertSingleSelfLabels [na, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertNoLabel ng
    assertNoLabel nx
    assertAssumptionsAre atms [na, nc, ne]
    assertContradictionsAre atms [nx]

  nb <- createNode atms "B" True False
  inGroup "Created Node B" $ do
    assertSingleSelfLabels [na, nb, nc, ne]
    assertSingleLabelEnvBy nh [nc, ne]
    assertNoLabel ng
    assertNoLabel nx
    assertAssumptionsAre atms [na, nb, nc, ne]
    assertContradictionsAre atms [nx]

  justifyNode "R4" nh [nb, nc]
  inGroup "Added Justification R4" $ do
    assertSingleSelfLabels [na, nb, nc, ne]
    assertNodeLabelAssumptions nh [[nc, ne], [nc, nb]]
    assertNoLabel ng
    assertNoLabel nx
    assertAssumptionsAre atms [na, nb, nc, ne]
    assertContradictionsAre atms [nx]
