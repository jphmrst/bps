{-# LANGUAGE TemplateHaskell, KindSignatures, RankNTypes #-}

module Data.TMS.ChooseDebugging (debugging) where
import Language.Haskell.TH
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

debuggingOn = True

unitQ :: Q Exp
unitQ = [| return () |]

monadIOQ = ''MonadIO
monadQ = ''Monad

-- | Macro which expands to definitions which either print debugging
-- statements, or do nothing.
debugging :: Q [Dec]
debugging = if debuggingOn
           then [d| class MonadIO m => Debuggable m
                    instance MonadIO m => Debuggable m
                    dbg :: Q Exp -> Q Exp
                    {-# INLINE dbg #-}
                    dbg exp = exp
                |]
           else [d| class Monad m => Debuggable m
                    instance Monad m => Debuggable m
                    dbg :: a -> Q Exp
                    {-# INLINE dbg #-}
                    dbg _ = unitQ
                |]
