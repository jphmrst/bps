{-|
Module      : TLT
Description : Testing in a monad transformer layer
Copyright   : (c) John Maraist, 2022
License     : AllRightsReserved
Maintainer  : haskell-tlt@maraist.org
Stability   : experimental
Portability : POSIX

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, for NON-COMMERCIAL use.  See the License for the specific
language governing permissions and limitations under the License.

-}

module Test.TLT (
  TestFail, Assertion,
  TLT, tlt,
  (~:), (~::), (~::-),
  (!==),  (!/=),  (!<),  (!>),  (!<=),  (!>=),
  (!==-), (!/=-), (!<-), (!>-), (!<=-), (!>=-)
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

-- * Results of tests

-- |Reasons why a test might fail
data TestFail = Asserted String | Erred

-- |An assertion is a computation in a `TLT` monad which return a list
-- of zero of more reasons for the failure of the assertion.  A
-- successful computation returns no reasons for failure.
type Assertion m = m [TestFail]

-- |Hierarchical structure holding the result of running tests,
-- possibly grouped into tests.
data TestResult = Test String [TestFail]
                | Group String [TestResult]

-- |Report the results of tests.
report :: [TestResult] -> IO ()
report trs = report' "" trs
  where report' ind trs = forM_ trs $ \ tr -> do
          case tr of
            Test s r -> putStrLn $ ind ++ "- " ++ s ++ ": " ++
                                     if null r then "passed" else "FAIL"
            Group s trs' -> do
              putStrLn $ ind ++ "- " ++ s ++ ":"
              report' ("  " ++ ind) trs'

-- |Accumulator for test results, in the style of a simplified Huet's
-- zipper which only ever adds to the end of the structure.
data TRBuf = Buf TRBuf String [TestResult] | Top [TestResult]

-- |Add a single test result to a `TRBuf`.
addResult :: TRBuf -> TestResult -> TRBuf
addResult (Top trs) tr = Top $ tr : trs
addResult (Buf up s trs) tr = Buf up s $ tr : trs

-- |Convert a `TRBuf` into a list of top-down `TestResult`s.
closeTRBuf :: TRBuf -> [TestResult]
closeTRBuf (Top ts) = reverse ts
closeTRBuf (Buf acc gname gtrs) =
  closeTRBuf $ closeWith acc $ Group gname $ reverse gtrs
  where closeWith (Top ts) g = Top $ g : ts
        closeWith (Buf acc gn gtrs) g = Buf acc gn $ g : gtrs

-- * The monad transformer in which test processes live

-- |Monad transformer for TLT tests.  This layer stores the results
-- from tests as they are executed.
newtype Monad m => TLT m r = TLT { unwrap :: StateT TRBuf m r }

-- |Using `TLT` as a functor.
instance (Monad m) => Functor (TLT m) where
  fmap f (TLT m) = TLT $ do
    v <- m
    return $ f v

-- |Using `TLT` as an applicative functor.
instance (Monad m, Functor m) => Applicative (TLT m) where
  pure v = TLT $ pure v
  (TLT m1) <*> (TLT m2) = TLT $ do
    f <- m1
    v <- m2
    return (f v)

-- |Standard `Monad`ic operations.
instance (Monad m, Functor m) => Monad (TLT m) where
  -- (>>=) :: TLT m a -> (a -> TLT m b) -> TLT m b
  (TLT m) >>= f = TLT $ m >>= (unwrap . f)

  -- (>>) :: TLT m a -> TLT m b -> TLT m b
  (TLT m1) >> (TLT m2) = TLT $ m1 >> m2

  -- return :: a -> TLT m a
  return v = TLT $ return v

-- |Allow the `TLT` layer to be used from a surrounding transformer.
instance MonadTrans TLT where
  lift = TLT . lift

-- |Facilitate `IO` interaction within or above the the `TLT` layer.
instance MonadIO m => MonadIO (TLT m) where
  liftIO = lift . liftIO

-- |Execute the tests specified in a `TLT` monad, and report the
-- results.
tlt :: MonadIO m => TLT m r -> m ()
tlt (TLT t) = do
  (_, resultsBuf) <- runStateT t $ Top []
  liftIO $ report $ closeTRBuf resultsBuf

-- * Specifying individual tests

infix 0 ~:
infix 1 ~::,  !==,  !/=,  !<,  !>,  !<=,  !>=
infix 1 ~::-, !==-, !/=-, !<-, !>-, !<=-, !>=-

-- |Name and test an assertion.
(~:) :: Monad m => String -> Assertion m -> TLT m ()
s ~: a = TLT $ do
  oldState <- get
  assessment <- lift a
  put $ addResult oldState $ Test s assessment

-- |Name and test a boolean value.
(~::-) :: Monad m => String -> Bool -> TLT m ()
s ~::- b = TLT $ do
  oldState <- get
  put $ addResult oldState $ Test s $
    if b then [] else [Asserted $ "Expected True but got False"]

-- |Name and test a boolean value.
(~::) :: Monad m => String -> m Bool -> TLT m ()
s ~:: bM = TLT $ do
  b <- lift bM
  oldState <- get
  put $ addResult oldState $ Test s $
    if b then [] else [Asserted $ "Expected True but got False"]

-- |Assert that two values are equal.
(!==-) :: (Monad m, Eq a, Show a) => a -> a -> Assertion m
exp !==- actual = return $
  if (exp == actual) then []
  else [Asserted $ "Expected " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a calculated value is as expected.
(!==) :: (Monad m, Eq a, Show a) => a -> m a -> Assertion m
exp !== actualM = do actual <- actualM
                     exp !==- actual

-- |Assert that two values are not equal.
(!/=-) :: (Monad m, Eq a, Show a) => a -> a -> Assertion m
exp !/=- actual = return $
  if (exp /= actual) then []
  else [Asserted $ "Expected different than " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a calculated value differs from some known value.
(!/=) :: (Monad m, Eq a, Show a) => a -> m a -> Assertion m
exp !/= actualM = do
  actual <- actualM
  exp !/=- actual

-- |Assert that a given boundary is strictly less than some value.
(!<-) :: (Monad m, Ord a, Show a) => a -> a -> Assertion m
exp !<- actual = return $
  if (exp < actual) then []
  else [Asserted $ "Lower bound (open) is " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.
(!<) :: (Monad m, Ord a, Show a) => a -> m a -> Assertion m
exp !< actualM = do
  actual <- actualM
  exp !<- actual

-- |Assert that a given boundary is strictly less than some value.
(!>-) :: (Monad m, Ord a, Show a) => a -> a -> Assertion m
exp !>- actual = return $
  if (exp > actual) then []
  else [Asserted $ "Upper bound (open) is " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.
(!>) :: (Monad m, Ord a, Show a) => a -> m a -> Assertion m
exp !> actualM = do
  actual <- actualM
  exp !<- actual

-- |Assert that a given boundary is strictly less than some value.
(!<=-) :: (Monad m, Ord a, Show a) => a -> a -> Assertion m
exp !<=- actual = return $
  if (exp <= actual) then []
  else [Asserted $ "Lower bound (closed) is " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.
(!<=) :: (Monad m, Ord a, Show a) => a -> m a -> Assertion m
exp !<= actualM = do
  actual <- actualM
  exp !<- actual

-- |Assert that a given boundary is strictly less than some value.
(!>=-) :: (Monad m, Ord a, Show a) => a -> a -> Assertion m
exp !>=- actual = return $
  if (exp >= actual) then []
  else [Asserted $ "Upper bound (closed) is " ++ show exp ++ " but got " ++ show actual]

-- |Assert that a given, constant boundary is strictly less than some
-- calculated value.
(!>=) :: (Monad m, Ord a, Show a) => a -> m a -> Assertion m
exp !>= actualM = do
  actual <- actualM
  exp !<- actual
