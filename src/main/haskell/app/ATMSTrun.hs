module ATMSTrun where

import Control.Monad.State
import Data.TMS.Formatters
import Data.TMS.ATMS.ATMST

-- Corresponds to Lisp function `book-1` in src/main/lisp/atms/atest.lisp
runATMS1 :: IO (Either AtmsErr ())
runATMS1 = do
  runATMST $ do
    do
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

    do
      liftIO $ putStrLn "========================================"
      atms <- createATMS "Ex2"
      setInformantStringViaString atms
      setDatumStringViaString atms
      na <- createNode atms "A" True False
      nb <- createNode atms "B" True False
      nc <- createNode atms "C" True False
      nd <- createNode atms "D" True False
      ne <- createNode atms "E" True False
      nf <- createNode atms "F" True False
      ng <- createNode atms "G" True False
      nh <- createNode atms "H" True False
      nm <- createNode atms "M" False False
      nn <- createNode atms "N" False False
      np <- createNode atms "P" False False
      nq <- createNode atms "Q" False False
      nr <- createNode atms "R" False False
      ns <- createNode atms "S" False False
      nk1 <- createNode atms "K1" False False
      nk2 <- createNode atms "K2" False False
      nk3 <- createNode atms "K3" False False
      justifyNode "R01" nm [na, nb]
      justifyNode "R02" nm [nc, nd]
      justifyNode "R03" nn [nb, nc]
      justifyNode "R04" np [nc]
      justifyNode "R05" nq [nc, nd, ne]
      justifyNode "R06" nr [nf, ng]
      justifyNode "R07" ns [nf, nh]
      justifyNode "R08" nk1 [nm, np]
      justifyNode "R09" nk1 [nn, nq]
      justifyNode "R10" nk2 [nn]
      justifyNode "R11" nk2 [np]
      justifyNode "R12" nk3 [nn, nq]
      justifyNode "R13" nk3 [nq, nr, ns]
      nx <- createNode atms "X" False True
      justifyNode "X" nx [nc, nf]

      debug atms

      liftIO $ putStrLn "----------------------------------------"
      i3 <- interpretations atms [[nk3]]
      liftIO $ putStrLn "Interpretations for (k3): "
      forM_ i3 $ debug
