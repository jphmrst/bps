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
import Data.List
import Data.Maybe
import Data.Foldable

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
    (Monad m, Monad (tmsMonad s m), Foldable k) =>
      String -> k (item s m) -> tmsMonad s m String
  formats sep = foldM folder ""
    where folder str itm = do
            itmStr <- format itm
            return $ itmStr ++ sep ++ str

  -- |Format several collections of artifacts, with the given string
  -- between each pair of collections, and a comma between each pair
  -- of artifacts.
  formatss ::
    (Monad m, Monad (tmsMonad s m), Foldable k1, Foldable k2) =>
      String -> k1 (k2 (item s m)) -> tmsMonad s m String
  formatss sep = foldM folder ""
    where folder str itms = do
            itmsStr <- formats "," itms
            return $ itmsStr ++ sep ++ str

  -- |Print a short representation of an artifact to the standard
  -- output.
  blurb :: (MonadIO m, MonadIO (tmsMonad s m)) => item s m -> tmsMonad s m ()
  blurb i = format i >>= liftIO . putStr

  -- |Print a short representation of a collection of artifacts to the
  -- standard output.
  blurbs ::
    (MonadIO m, MonadIO (tmsMonad s m), Foldable k) =>
      String -> k (item s m) -> tmsMonad s m ()
  blurbs sep xs = formats sep xs >>= liftIO . putStr

  -- |Print a short representation of a collection of collections of
  -- artifacts to the standard output.
  blurbss ::
    (MonadIO m, MonadIO (tmsMonad s m), Foldable k1, Foldable k2) =>
      String -> k1 (k2 (item s m)) -> tmsMonad s m ()
  blurbss sep xs = formatss sep xs >>= liftIO . putStr

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

-- |Class of artifacts which can be printed for the purpose of
-- debugging in a `MonadIO` to the standard output.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class Debugged item tmsMonad where
  debug :: (MonadIO m) => item s m -> tmsMonad s m ()
