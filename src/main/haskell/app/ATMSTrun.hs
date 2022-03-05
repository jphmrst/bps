module ATMSTrun where

import Data.Symbol
import Data.TMS.ATMS.ATMST

runATMS1 :: IO (Either AtmsErr ())
runATMS1 = do
  runATMST $ do
    atms <- createATMS "Ex1"
    na <- createNode atms "A" True False
    nc <- createNode atms "C" True False
    ne <- createNode atms "E" True False
    nh <- createNode atms "H" False False
    justifyNode "R1" nh [nc, ne]
    printAtms atms
