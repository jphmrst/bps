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
import Data.Foldable

-- |Class of artifacts which can be given a short formatted
-- description in a particular monad.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class TmsFormatted item tmsMonad where
  -- |Format a single artifact.
  tmsFormat :: (Monad m) => item s m -> tmsMonad s m String

  -- |Format several artifacts, with the given string between each
  -- pair.
  tmsFormats ::
    (Monad m, Monad (tmsMonad s m), Foldable k) =>
      String -> k (item s m) -> tmsMonad s m String
  tmsFormats sep = foldM folder ""
    where folder str itm = do
            itmStr <- tmsFormat itm
            return $ itmStr ++ sep ++ str

  -- |Format several collections of artifacts, with the given string
  -- between each pair of collections, and a comma between each pair
  -- of artifacts.
  tmsFormatss ::
    (Monad m, Monad (tmsMonad s m), Foldable k1, Foldable k2) =>
      String -> k1 (k2 (item s m)) -> tmsMonad s m String
  tmsFormatss sep = foldM folder ""
    where folder str itms = do
            itmsStr <- tmsFormats "," itms
            return $ itmsStr ++ sep ++ str

  -- |Print a short representation of an artifact to the standard
  -- output.
  tmsBlurb :: (MonadIO m, MonadIO (tmsMonad s m)) => item s m -> tmsMonad s m ()
  tmsBlurb i = tmsFormat i >>= liftIO . putStr

  -- |Print a short representation of a collection of artifacts to the
  -- standard output.
  tmsBlurbs ::
    (MonadIO m, MonadIO (tmsMonad s m), Foldable k) =>
      String -> k (item s m) -> tmsMonad s m ()
  tmsBlurbs sep xs = tmsFormats sep xs >>= liftIO . putStr

  -- |Print a short representation of a collection of collections of
  -- artifacts to the standard output.
  tmsBlurbss ::
    (MonadIO m, MonadIO (tmsMonad s m), Foldable k1, Foldable k2) =>
      String -> k1 (k2 (item s m)) -> tmsMonad s m ()
  tmsBlurbss sep xs = tmsFormatss sep xs >>= liftIO . putStr

-- |Class of artifacts which can be printed in a `MonadIO` to the
-- standard output, possibly over multiple lines, but in a terse
-- representation shorter than for debugging.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class TmsPrinted item tmsMonad where
  tmsPrint :: (MonadIO m) => item s m -> tmsMonad s m ()

-- |Class of artifacts which can be printed for the purpose of
-- debugging in a `MonadIO` to the standard output.
--
-- This is a multi-parameter class: the first argument is the artifact
-- type; the second, the monad.  Both type constructors take two
-- arguments, the state thread type used by the TMS thread, and the
-- enclosed monad constructor.
class TmsDebugged item tmsMonad where
  tmsDebug :: (MonadIO m) => item s m -> tmsMonad s m ()


