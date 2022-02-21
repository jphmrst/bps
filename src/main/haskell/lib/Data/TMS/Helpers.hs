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

-- * Even more loops

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

-- * Mutable lists (cons cells) in `STT`

data MList s a = MCons (STRef s a) (STRef s (MList s a)) | MNil

mnull MNil = True
mnull _ = False

mcar (MCons x _)  = x
mcdr (MCons _ xs) = xs

fromList :: Monad m => [a] -> STT s m (MList s a)
fromList [] = return MNil
fromList (x : xs) = do
  car <- newSTRef x
  tail <- fromList xs
  cdr <- newSTRef tail
  return $ MCons car cdr

toList :: Monad m => MList s a -> STT s m [a]
toList MNil = return []
toList (MCons car cdr) = do
  x <- readSTRef car
  ms <- readSTRef cdr
  xs <- toList ms
  return $ x : xs

-- |Treating an `MList` as a stack, add a new element at the top of
-- the stack, and return the new stack top.
mlistPush :: Monad m => a -> MList s a -> STT s m (MList s a)
mlistPush item mlist = do
  itemRef <- newSTRef item
  mlistRef <- newSTRef mlist
  return $ MCons itemRef mlistRef

-- |Iterate over the elements of a `MList`.  The body does not
-- necessarily need operate in the same monad as where the references
-- originate; the @lifter@ parameter brings the latter into the
-- former.
mlistFor_ :: (Monad m0, Monad m) =>
  (forall r . STT s m0 r -> m r) -> MList s a -> (a -> m ()) -> m ()
mlistFor_ lifter MNil _ = return ()
mlistFor_ lifter (MCons xref xsref) bodyf = do
  x <- lifter $ readSTRef xref
  bodyf x
  xs <- lifter $ readSTRef xsref
  mlistFor_ lifter xs bodyf

-- |Like `mlistFor_`, but the body expects an `MCons` cell instead of
-- the list element itself.  Useful for mutating the list along the
-- way.
mlistForCons_ :: (Monad m0, Monad m) =>
  (forall r . STT s m0 r -> m r) -> MList s a -> (MList s a -> m ()) -> m ()
mlistForCons_ _ MNil _ = return ()
mlistForCons_ lifter mc@(MCons _ _) bodyf = do
  bodyf mc
  xs <- lifter $ readSTRef (mcdr mc)
  mlistForCons_ lifter xs bodyf

-- |A combination of `mlistForCons_` and `forMwhile_`: iterate over
-- the `MCons` cell of a list, with a trigger for an early exit.  Note
-- that the monad for the continuation condition is over the overall
-- monad @m@, not the `STT` wrapped monad @m0@.
mlistForConsWhile_ ::
  (Monad m0, Monad m) =>
    (forall r . STT s m0 r -> m r) -> m Bool -> MList s a -> (MList s a -> m ())
      -> m ()
mlistForConsWhile_ _ _ MNil _ = return ()
mlistForConsWhile_ lifter moreM mc@(MCons _ _) bodyf =
  whenM moreM $ do
    bodyf mc
    xs <- lifter $ readSTRef (mcdr mc)
    mlistForConsWhile_ lifter moreM xs bodyf

