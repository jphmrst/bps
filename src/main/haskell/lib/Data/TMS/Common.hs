{-|
Module      : Common
Description : Types and functions shared by several TMS implementations
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

{-# LANGUAGE FlexibleInstances #-}

module Data.TMS.Common where

import Data.Symbol

-- |Class of type which can be used as the datum underlying a @Node@
-- in a TMS.
class NodeDatum d where
  -- |The datum associated with the contradiction node in a
  -- newly-initialized `ATMS` with `Node` data of this type.
  contradictionNodeDatum :: d

instance NodeDatum String where
  contradictionNodeDatum = "The contradiction"
instance NodeDatum Symbol where
  contradictionNodeDatum = intern "The contradiction"
