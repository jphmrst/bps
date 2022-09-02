module ATMSTrun where

import Control.Monad.State
import Data.TMS.Formatters
import Data.TMS.ATMS.ATMST

-- Corresponds to Lisp function `book-1` in src/main/lisp/atms/atest.lisp
runATMS1 :: IO (Either AtmsErr ())
runATMS1 = do
  runATMST $ do
    atms <- createATMS "Ex1"
    setInformantStringViaString atms
    setDatumStringViaString atms
    -- debugAtms "Created" atms
    na <- createNode atms "A" True False
    -- debugAtms "Added assumption node A" atms
    nc <- createNode atms "C" True False
    -- debugAtms "Added assumption node C" atms
    ne <- createNode atms "E" True False
    -- debugAtms "Added assumption node E" atms
    nh <- createNode atms "H" False False
    -- debugAtms "Added non-assumption node H" atms
    justifyNode "R1" nh [nc, ne]
    -- debugAtms "After rule R1" atms
    ng <- createNode atms "G" False False
    -- debugAtms "After non-assumption node G" atms
    justifyNode "R2" ng [na, nc]
    -- debugAtms "After rule R2" atms
    nx <- createNode atms "X" False True
    -- debugAtms "After contradiction node X" atms
    justifyNode "R3" nx [ng]
    -- debugAtms "After rule R3" atms
    nb <- createNode atms "B" True False

    liftIO $ putStrLn "Added assumption node B"
    debug atms
    justifyNode "R4" nh [nb, nc]
    liftIO $ putStrLn "After rule R4"
    debug atms

    liftIO $ putStrLn "----------------------------------------"
    i1 <- interpretations atms [[na, nc], [nh, ng]]
    liftIO $ putStrLn "Interpretations for (a, c); (h, g): "
    forM_ i1 $ debug

    liftIO $ putStrLn "----------------------------------------"
    i2 <- interpretations atms [[nh, ng]]
    liftIO $ putStrLn "Interpretations for (h, g): "
    forM_ i2 $ debug

    liftIO $ putStrLn "----------------------------------------"
    i3 <- interpretations atms [[nh]]
    liftIO $ putStrLn "Interpretations for (h): "
    forM_ i3 $ debug
