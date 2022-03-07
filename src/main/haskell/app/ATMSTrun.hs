module ATMSTrun where

import Data.Symbol
import Data.TMS.ATMS.ATMST

runATMS1 :: IO (Either AtmsErr ())
runATMS1 = do
  runATMST $ do
    atms <- createATMS "Ex1"
    setInformantStringViaString atms
    setDatumStringViaString atms
    debugAtms "Created" atms
    na <- createNode atms "A" True False
    debugAtms "Added assumption node A" atms
    nc <- createNode atms "C" True False
    debugAtms "Added assumption node C" atms
    ne <- createNode atms "E" True False
    debugAtms "Added assumption node E" atms
    nh <- createNode atms "H" False False
    debugAtms "Added non-assuption node H" atms
    justifyNode "R1" nh [nc, ne]
    debugAtms "After rule R1" atms
