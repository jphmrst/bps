{-|
Module      : Formatters
Description : Formatting functions, consistently named across the TMS modules
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

{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.Formatters where

import Control.Monad
import Control.Monad.State
import Control.Monad.Extra
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Traversable

mapAndIntercalateM ::
  (Monad m, Traversable t) => (a -> m String) -> String -> t a -> m String
mapAndIntercalateM f sep xs = do
  ys <- mapM f xs
  return $ foldl1 (\s1 s2 -> s1 ++ sep ++ s2) ys

-- |Class of artifacts which can be given a short formatted
-- description in a particular monad.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class Formatted item tmsMonad where
  -- |Format a single artifact.
  format :: (Monad m) => item s m -> tmsMonad s m String

  -- |Format several artifacts, with the given string between each
  -- pair.
  formats ::
    (Monad m, Monad (tmsMonad s m), Traversable k) =>
      String -> String -> k (item s m) -> tmsMonad s m String
  formats none _ xs | null xs = return none
  formats _ sep xs = mapAndIntercalateM format sep xs

  -- |Format several collections of artifacts, with the given string
  -- between each pair of collections, and a comma between each pair
  -- of artifacts.
  formatss ::
    (Monad m, Monad (tmsMonad s m), Traversable k1, Traversable k2) =>
      String -> String -> String -> String -> k1 (k2 (item s m)) ->
        tmsMonad s m String
  formatss outNone _ _ _ xs | null xs = return outNone
  formatss outNone outSep inNone inSep xs =
    mapAndIntercalateM (formats inNone inSep) outSep xs

  -- |Print a short representation of an artifact to the standard
  -- output.
  blurb :: (MonadIO m, MonadIO (tmsMonad s m)) => item s m -> tmsMonad s m ()
  blurb i = format i >>= liftIO . putStr

  -- |Print a short representation of a collection of artifacts to the
  -- standard output.
  blurbs ::
    (MonadIO m, MonadIO (tmsMonad s m), Traversable k) =>
      String -> String -> k (item s m) -> tmsMonad s m ()
  blurbs none sep xs = formats none sep xs >>= liftIO . putStr

  -- |Print a short representation of a collection of collections of
  -- artifacts to the standard output.
  blurbss ::
    (MonadIO m, MonadIO (tmsMonad s m), Traversable k1, Traversable k2) =>
      String -> String -> String -> String ->
        k1 (k2 (item s m)) -> tmsMonad s m ()
  blurbss outNone outSep inNone inSep xs =
    formatss outNone outSep inNone inSep xs >>= liftIO . putStr

-- |Class of artifacts which can be printed in a `MonadIO` to the
-- standard output, possibly over multiple lines, but in a terse
-- representation shorter than for debugging.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class Printed item tmsMonad where
  pprint :: (MonadIO m) => item s m -> tmsMonad s m ()

  pprints ::
    (MonadIO m, MonadIO (tmsMonad s m)) => [item s m] -> tmsMonad s m ()
  pprints items = forM_ items $ \i -> pprint i

  pprintss ::
    (MonadIO m, MonadIO (tmsMonad s m)) => [[item s m]] -> tmsMonad s m ()
  pprintss itemss = forM_ itemss $ \i -> pprints i

-- |Class of artifacts which can be printed for the purpose of
-- debugging in a `MonadIO` to the standard output.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class Debugged item tmsMonad where
  debug :: (MonadIO m) => item s m -> tmsMonad s m ()
