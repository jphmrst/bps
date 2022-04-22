{-# LANGUAGE TemplateHaskell, KindSignatures, RankNTypes #-}

{-|
Module      : ChooseDebugging
Description : The main switch for activating tracing messages for debugging in output.
Copyright   : (c) John Maraist, 2022
License     : AllRightsReserved
Maintainer  : haskell-tms@maraist.org
Stability   : experimental
Portability : POSIX

The module contains the flag which indicates whether debugging output
should be compiled into modules using this system for runtime trace
output.
-}

module Data.TMS.ChooseDebugging (debuggingOn, debugging) where
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

-- | Flag which indicates whether debugging output should be compiled
-- into modules using this system for runtime trace output.
debuggingOn = False

unitQ :: Q Exp
{-# INLINE unitQ #-}
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
