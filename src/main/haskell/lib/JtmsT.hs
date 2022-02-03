module JtmsT where
import Control.Monad.State
import Control.Monad.ST.Trans

data JS = JS

type JTmsT s m a = StateT JS (STT s m) a
