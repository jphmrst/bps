module JTMSrun where

import Data.Symbol
import Data.TMS.JTMS
import Control.Monad.IO.Class

runJTMS1 :: IO (Either JtmsErr ())
runJTMS1 = runJTMST $ do
  j <- createJTMS "Ex1"
  na <- createNode j (intern "a") True False
  naName <- nodeString na
  naIn <- isInNode na
  liftIO $ putStrLn $
    "Node " ++ naName ++ " is " ++ if naIn then "in" else "out"
