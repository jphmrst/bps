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
import Control.Monad.IO.Class
import Test.HUnit
import Test.TLT

main :: IO ()
main = do
  runJTMST $ do
    counts <- testEx1
    liftIO $ putStrLn $ show counts
  putStrLn "Test run completed"


-- Prints result of the string length calculation.
report :: Either JtmsErr () -> IO ()
report (Right _) = putStrLn ("Tests passed")
report (Left e) = putStrLn ("Caught exception: " ++ (show e))

-- ex1 :: Monad m => forall s . JTMST s m (JTMS Symbol String Void s m)
ex1 = do
  j <- createJTMS "Ex1"
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

-- testEx1 :: Monad m => JTMS Symbol String Void s m -> JTMST s m ()
testEx1 = do
  (jtms, na, nb, nc, nd, ne, nf, ng) <- ex1

  enableAssumption na
  ba1 <- isInNode na
  bb1 <- isInNode nb
  bc1 <- isInNode nc
  bd1 <- isInNode nd
  be1 <- isInNode ne
  bf1 <- isInNode nf
  bg1 <- isInNode ng
  liftIO $ runTestTT $ TestList [
    TestCase $ assertBool "a believed" ba1,
    "2 is 2" Test.HUnit.~: 2 @=? 2
    ]
