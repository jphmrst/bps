{-|
Description : Testing truth maintenance systems (TMSes)
Copyright   : (c) John Maraist, 2022
              Kenneth D. Forbus, Johan de Kleer and Xerox Corporation, 1986-1993
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Testing the translation of Forbus and de Kleer's various truth
maintenance systems (TMSes) from Common Lisp to Haskell.

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

import Data.TMS.ATMS.ATMST
import Data.TMS.LTMS
import Data.TMS.JTMS
import Control.Monad.ST.Trans
import Test.TLT
import JTMSTests
import ATMSTests
import LTMSTests

main :: IO ()
main = do
  runSTT $ tlt $ do
    inGroup "JTMS tests" $ runJTMST $ do
      testEx1
      testEx3
    inGroup "ATMS tests" $ runATMST $ do
      ex1AndTest
    inGroup "LTMS tests" $ runLTMST $ do
      inGroup "LTMS Test 1" $ do
        ltmsTest1
