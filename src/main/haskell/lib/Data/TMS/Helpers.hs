{-|
Module      : Helpers
Description : Helping functions, sort of ExtraExtra
Copyright   : (c) John Maraist, 2022
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied, for NON-COMMERCIAL use.  See the License for the specific
language governing permissions and limitations under the License.

-}

{-# LANGUAGE RankNTypes #-}

module Data.TMS.Helpers where

import Control.Monad.State
import Control.Monad.ST.Trans
import Control.Monad.Except
import Control.Monad.Extra
import Data.List

-- * Ordered lists

-- | Determine whether one list is a subset of the other, under the
-- assumption that both lists are sorted in ascending order.
ordSubsetp :: Ord a => [a] -> [a] -> Bool
ordSubsetp [] _ = True
ordSubsetp (_ : _) [] = False
ordSubsetp l1@(n1 : ns1) l2@(n2 : ns2) =
  case compare n1 n2 of
    LT -> False
    EQ -> ordSubsetp ns1 ns2
    GT -> ordSubsetp l1 ns2

-- * Even more loops

-- | Convert a list to a string, where the converter for each element
-- is a monadic computation.
formatList :: Monad m => String -> (a -> m String) -> [a] -> m String
formatList s f xs = mapM f xs >>= return . intercalate s

-- | Like `forM_`, but with both the elements source as well as the
-- loop body as computations over the monad.
forMM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m ()) -> m ()
forMM_ srcM f = do
  src <- srcM
  forM_ src f

-- | Like `forM_`, but with an extra check run after the body of the
-- loop.  If the check fails, the loop exits early.
forMwhile_ :: Monad m => [a] -> m Bool -> (a -> m ()) -> m ()
forMwhile_ [] _ _ = return ()
forMwhile_ (x : xs) pred bodyf = do
  whenM pred $ do
    bodyf x
    forMwhile_ xs pred bodyf

-- | Like `forMwhile_`, but the source list is also the result of a
-- monadic computation.
forMMwhile_ :: Monad m => m [a] -> m Bool -> (a -> m ()) -> m ()
forMMwhile_ xsM condM bodyf = do
  xs <- xsM
  forMwhile_ xs condM bodyf

-- | Like `forMM_`, except instead of a fixed list, loop over `Maybe`
-- values returned from a subcomputation, until that subcomputation
-- returns `Nothing`.
whileReturnJust :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileReturnJust gen f = do
  res <- gen
  case res of
    Nothing -> return ()
    Just x  -> do
      f x
      whileReturnJust gen f

-- | Like `unless`, expect both the tested value and the body are
-- returned from a computation in a monad.
unlessMM :: Monad m => m Bool -> m () -> m ()
unlessMM cnd body = whenM (notM cnd) body

-- * Lists under references in the `STS` monad transformer

-- |Monadic version of @null@ for a list stored in an `STRef`: returns
-- `True` when the list is empty.
nullR :: Monad m => STRef s [a] -> STT s m Bool
nullR ref = do
  xs <- readSTRef ref
  return $ null xs

-- |Opposite of `nullR`, returning `False` when the referenced list is
-- empty.
nonnullR :: Monad m => STRef s [a] -> STT s m Bool
nonnullR ref = do
  xs <- readSTRef ref
  return $ not $ null xs

-- |Like a combination of `whenM` and `nonnullR`, where the body
-- receives the (pure) non-null list as an argument.
whenNonnullR :: (Monad m0, Monad m) =>
  (forall r . STT s m0 r -> m r) -> STRef s [a] -> ([a] -> m ()) -> m ()
whenNonnullR lifter ref bodyf = do
  xs <- lifter $ readSTRef ref
  if (null xs) then return () else bodyf xs

-- |Map over the values contained within a list of references.
mapRefs :: Monad m => (a -> b) -> [STRef s a] -> STT s m [b]
mapRefs f [] = return []
mapRefs f (xr : xrs) = do
  x <- readSTRef xr
  xs' <- mapRefs f xrs
  return $ f x : xs'

-- |Fold (right-associatively) the values contained within a list of
-- references.
foldrRefs :: Monad m => (a -> b -> b) -> b -> [STRef s a] -> STT s m b
foldrRefs f z [] = return z
foldrRefs f z (xr : xrs) = do
  x <- readSTRef xr
  z' <- foldrRefs f z xrs
  return $ f x z'

-- |Fold (left-associatively) the values contained within a list of
-- references.
foldlRefs :: Monad m => (b -> a -> b) -> b -> [STRef s a] -> STT s m b
foldlRefs f z [] = return z
foldlRefs f z (xr : xrs) = do
  x <- readSTRef xr
  foldlRefs f (f z x) xrs

-- ** Stack-like operations

-- |Push a value onto the front of the list at the given `STT`
-- reference.
push :: Monad m => a -> STRef s [a] -> STT s m ()
push v r = do
  prev <- readSTRef r
  writeSTRef r $ v : prev

-- |Push the result of a computation onto the front of the list at the
-- given `STT` reference.
pushM :: Monad m => m a -> STRef s [a] -> STT s m ()
pushM m r = do
  v <- lift m
  push v r

-- |Push every value in a collection onto the front of the list at the
-- given `STT` reference.
pushAll :: (Monad m, Traversable t) => t a -> STRef s [a] -> STT s m ()
pushAll vs r = forM_ vs $ \v -> push v r

-- |Push every value in a collection returned from a computation onto
-- the front of the list at the given `STT` reference.
pushAllM :: (Monad m, Traversable t) => m (t a) -> STRef s [a] -> STT s m ()
pushAllM m r = do
  vs <- lift m
  pushAll vs r

-- |Pop a value from the given reference to a list if one exists.
pop :: Monad m => STRef s [a] -> STT s m (Maybe a)
pop queue = do
  queueList <- readSTRef queue
  case queueList of
    [] -> return Nothing
    (x : xs) -> do
      writeSTRef queue xs
      return $ Just x

-- |Consumes the elements of a referenced list, one at a time, until
-- the list is empty.  The first argument is a @lift@-style function
-- which brings `STT` operations into the top-level monad of interest.
-- Intended to be compatible with stack-like behavior (such as with
-- `push`; this function does use `pop`) where the body of the loop
-- may add elements.
whileListM_ :: (Monad m0, Monad m) =>
  (forall r . STT s m0 r -> m r) -> STRef s [a] -> (a -> m ()) -> m ()
whileListM_ lifter listRef bodyf = whileListM_'
  where whileListM_' = do
          top <- lifter $ pop listRef
          case top of
            Nothing -> return ()
            Just x -> do
              bodyf x
              whileListM_'

-- * Strings

-- |Form a comma-separated string from a list.
commaList :: (a -> String) -> [a] -> String
commaList f [] = ""
commaList f xs = foldl1 (\ x y -> x ++ ", " ++ y) $ map f xs

unmaybe [] = []
unmaybe (Just a : xs) = a : unmaybe xs
unmaybe (_ : xs) = unmaybe xs
