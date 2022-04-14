{-# LANGUAGE TemplateHaskell, KindSignatures, RankNTypes, FlexibleInstances, UndecidableInstances #-}

module Data.TMS.Dbg (
  -- |Constraint for monads which will require `MonadIO` when
  -- debugging is activated.
  Debuggable,
  -- |The quoted code will be run when debugging is activated.
  dbg
  ) where
import Language.Haskell.TH
import Data.TMS.ChooseDebugging (debugging)
import Control.Monad.IO.Class

$(debugging)
