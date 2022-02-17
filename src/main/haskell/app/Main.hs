module Main where

import Data.Symbol
import Data.TMS.JTMS
import Control.Monad.IO.Class

main :: IO ()
main =
  let testSym = intern "test"
  in do
    liftIO $ putStrLn $ show testSym
    runJTMST $ do
      j <- createJTMS "Ex1"
      na <- createNode j (intern "a") True False
      naName <- nodeString na
      naIn <- isInNode na
      liftIO $ putStrLn $
        "Node " ++ naName ++ " is " ++ if naIn then "in" else "out"
    return ()
