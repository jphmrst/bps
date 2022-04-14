{-|
Module      : MList
Description : Mutable linked lists in STT
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

module Data.TMS.MList where

import Control.Monad.State
import Control.Monad.ST.Trans
import Control.Monad.Except
import Control.Monad.Extra

-- * Mutable lists (cons cells) in `STT`

-- |Singly linked lists!  But with mutable CARs and CDRs à la Common
-- Lisp.
data MList s a = MCons (STRef s a) (STRef s (MList s a))
                 -- ^ A @cons@ cell with mutable fields.
               | MNil
                 -- ^ Regular old @nil@.

toMList :: Monad m => [a] -> STT s m (MList s a)
toMList [] = return MNil
toMList (x : xs) = do
  car <- newSTRef x
  cdrBody <- toMList xs
  cdr <- newSTRef cdrBody
  return $ MCons car cdr

-- |Convert an `MList` to a `String`.
showM :: (Show a, Monad m) => MList s a -> STT s m String
showM MNil = return "[]"
showM (MCons xr xsr) = do
  x <- readSTRef xr
  xs <- readSTRef xsr
  let sx = show x
  sxs <- showM xs
  return $ sx ++ " m: " ++ sxs

-- |Returns `True` for an empty list.
mnull MNil = True
mnull _ = False

-- |Returns `True` from an `STT` monad for a reference to an empty
-- list.
getMnull :: Monad m => STRef s (MList s a) -> STT s m Bool
getMnull ref = readSTRef ref >>= return . mnull

-- |Returns the CAR (element) of the first CONS cell of a non-empty
-- mutable list.
mcar (MCons x _)  = readSTRef x
-- |Returns the CDR (next cell) of the first CONS cell of a non-empty
-- mutable list.
mcdr (MCons _ xs) = readSTRef xs

-- |Convert a traditional Haskell list into a mutable `MList` list.
mlength :: Monad m => MList s a -> STT s m Int
mlength MNil = return 0
mlength (MCons _ xs) = do
  cdr <- readSTRef xs
  cdrLen <- mlength cdr
  return $ 1 + cdrLen

-- |Convert a traditional Haskell list into a mutable `MList` list.
fromList :: Monad m => [a] -> STT s m (MList s a)
fromList [] = return MNil
fromList (x : xs) = do
  car <- newSTRef x
  tail <- fromList xs
  cdr <- newSTRef tail
  return $ MCons car cdr

-- |Convert a traditional Haskell list into a mutable `MList` list,
-- applying the given function to each element.
fromListMap :: Monad m => (a -> b) -> [a] -> STT s m (MList s b)
fromListMap _ [] = return MNil
fromListMap f (x : xs) = do
  car <- newSTRef $ f x
  tail <- fromListMap f xs
  cdr <- newSTRef tail
  return $ MCons car cdr

-- |Convert a mutable `MList` list into a traditional Haskell list.
toList :: Monad m => MList s a -> STT s m [a]
toList MNil = return []
toList (MCons car cdr) = do
  x <- readSTRef car
  ms <- readSTRef cdr
  xs <- toList ms
  return $ x : xs

-- |Convert a mutable `MList` list of `Maybe` values into a
-- traditional Haskell list containing only the values under a `Just`
-- constructor.
toUnmaybeList :: Monad m => MList s (Maybe a) -> STT s m [a]
toUnmaybeList MNil = return []
toUnmaybeList (MCons car cdr) = do
  xmaybe <- readSTRef car
  ms <- readSTRef cdr
  xs <- toUnmaybeList ms
  case xmaybe of
    Nothing -> return xs
    Just x -> return $ x : xs

-- |A version of @map@ for `MList`s.
mlistMap :: Monad m => (a -> b) -> MList s a -> STT s m (MList s b)
mlistMap f MNil = return MNil
mlistMap f (MCons xref xsref) = do
  x <- readSTRef xref
  xs <- readSTRef xsref
  xref' <- newSTRef $ f x
  xs' <- mlistMap f xs
  xsref' <- newSTRef xs'
  return $ MCons xref' xsref'

-- |A version of @filter@ for `MList`s.
mlistFilter :: Monad m => (a -> Bool) -> MList s a -> STT s m (MList s a)
mlistFilter p l = do
  (_, result) <- flt p l
  return result
  where
    flt :: Monad m => (a -> Bool) -> MList s a -> STT s m (Bool, MList s a)
    flt pred l@MNil = return (False, l)
    flt pred l@(MCons xref xsref) = do
      x <- readSTRef xref
      xs <- readSTRef xsref
      (changed, xs') <- flt pred xs
      if pred x
      then if changed
           then do
             xsref' <- newSTRef xs'
             return (True, MCons xref xsref')
           else return (False, l)
      else return (True, xs')

-- |Return a new `MList` which strips off the `Just` constructor from
-- its elements, dropping and elements which are `Nothing`.
mlistUnmaybe :: Monad m => MList s (Maybe a) -> STT s m (MList s a)
mlistUnmaybe MNil = return MNil
mlistUnmaybe (MCons xref xsref) = do
  x <- readSTRef xref
  xs <- readSTRef xsref
  xs' <- mlistUnmaybe xs
  case x of
    Nothing -> return xs'
    Just x' -> do
      xref' <- newSTRef x'
      xsref' <- newSTRef xs'
      return $ MCons xref' xsref'

-- |Return a new `MList` which drops elements which are `Nothing`.
mlistStripNothing :: Monad m => MList s (Maybe a) -> STT s m (MList s (Maybe a))
mlistStripNothing = mlistFilter (not . null)

-- |Return a new `MList` which drops elements which are `Nothing` from
-- the `MList` under the reference argument.
getMlistStripNothing ::
  Monad m => STRef s (MList s (Maybe a)) -> STT s m (MList s (Maybe a))
getMlistStripNothing ref = do
  mlist <- readSTRef ref
  mlistFilter (not . null) mlist

-- |Treating an `MList` as a stack, add a new element at the top of
-- the stack, and return the new stack top.
mlistPush :: Monad m => a -> MList s a -> STT s m (MList s a)
mlistPush item mlist = do
  itemRef <- newSTRef item
  mlistRef <- newSTRef mlist
  return $ MCons itemRef mlistRef

-- |Treating an `MList` as a stack, add a new element at the top of
-- the stack, and return the new stack top.
mlistRefPush :: Monad m => a -> STRef s (MList s a) -> STT s m ()
mlistRefPush item mlistRef = do
  carRef <- newSTRef item
  cdr <- readSTRef mlistRef
  newCdrRef <- newSTRef cdr
  let newCons = MCons carRef newCdrRef
  writeSTRef mlistRef newCons

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
  xs <- lifter $ mcdr mc
  mlistForCons_ lifter xs bodyf

-- |A combination of `mlistForCons_` and
-- `Data.TMS.Helpers.forMwhile_`: iterate over the `MCons` cell of a
-- list, with a trigger for an early exit.  Note that the monad for
-- the continuation condition is over the overall monad @m@, not the
-- `STT` wrapped monad @m0@.
mlistForConsWhile_ ::
  (Monad m0, Monad m) =>
    (forall r . STT s m0 r -> m r) -> MList s a -> m Bool -> (MList s a -> m ())
      -> m ()
mlistForConsWhile_ _ MNil _ _ = return ()
mlistForConsWhile_ lifter mc@(MCons _ _) moreM bodyf =
  whenM moreM $ do
    bodyf mc
    xs <- lifter $ mcdr mc
    mlistForConsWhile_ lifter xs moreM bodyf

-- |Overwrite the @car@ slot of the given `MCons` with the given
-- value.  Named after the Common Lisp function with the same
-- behavior.
rplaca :: Monad m => MList s a -> a -> STT s m ()
rplaca (MCons r _) v = writeSTRef r v
