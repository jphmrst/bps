{-# LANGUAGE TemplateHaskell, KindSignatures, RankNTypes, FlexibleInstances, UndecidableInstances #-}

module Data.TMS.Dbg (Debuggable, dbg) where
import Language.Haskell.TH
import Data.TMS.ChooseDebugging (debugging)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.Writer.Lazy as WL
import qualified Control.Monad.Trans.Writer.Strict as WS

$(debugging)
