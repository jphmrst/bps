{-# LANGUAGE RankNTypes #-}

module Main where

import ATMSTrun
import JTMSrun
import Control.Monad.Random
import Control.Monad.Random.Class
import Data.TMS.ATMS.ATMST

main :: IO ()
main = do
  -- gen <- getStdGen
  let gen = mkStdGen 8675309 --- Fix for comparing like to like
  evalRandT (runATMST $
              makeForceATMS
                (IntRange 800 810) (IntRange 4000 4010)
                0.1
                (IntRange 50 60) (IntRange 25 30)
                False) gen
  return ()

intSet :: (RandomGen g, Monad m) => Int -> Int -> RandT g m [Int]
intSet 0 _ = return []
intSet n m = do
  x <- getRandomR (0, m)
  xs <- intSet (n - 1) m
  return $ x : xs

intSetExcept :: (RandomGen g, Monad m) => Int -> Int -> Int -> RandT g m [Int]
intSetExcept 0 _ _ = return []
intSetExcept n m d = do
  x <- getRandomR (0, m)
  if (x == d) then intSetExcept n m d else do
    xs <- intSetExcept (n - 1) m d
    return $ x : xs

data IntRange = IntRange { lo :: Int, hi :: Int }

sample :: (RandomGen g, Monad m) => IntRange -> RandT g m Int
sample (IntRange lo hi) = getRandomR (lo, hi)

coinFlip :: (RandomGen g, Monad m) => Double -> RandT g m Bool
coinFlip p = do
  q <- getRandomR (0.0, 1.0)
  return $ q <= p

makeForceATMS ::
  (RandomGen g, MonadIO m) =>
    IntRange -> IntRange -> Double -> IntRange -> IntRange -> Bool ->
      ATMST s (RandT g m) ()
makeForceATMS assumptionsRange nonassumptionsRange contradictionChance
              justificationsPerConclusion antecedentsPerJustifications
              cyclic = do

  atms <- createATMS "Random ATMS"
  setDatumStringViaString atms
  setInformantStringViaString atms
  assumptions <- lift $ sample assumptionsRange
  nonassumptions <- lift $ sample nonassumptionsRange
  let totalNodes = assumptions + nonassumptions

  assumptionNodes <- forM [0 .. assumptions - 1] $ \i ->
    createNode atms ("Node-" ++ show i) True False

  nonassumptionNodes <- forM [0 .. nonassumptions - 1] $ \i -> do
    isContradiction <- lift $ coinFlip contradictionChance
    let idx = assumptions + i
    createNode atms ("Node-" ++ show idx) False isContradiction

  let nodes = assumptionNodes ++ nonassumptionNodes

  {-# SCC "mainLoop" #-} forM_ [0 .. nonassumptions - 1] $ \i -> do
    let idx = assumptions + i
    let node = nodes !! idx
    justifications <- lift $ sample justificationsPerConclusion
    -- lift $ lift $ liftIO $ putStrLn $
    --   show justifications ++ " justifications for node " ++ show idx
    forM_ [0 .. justifications - 1] $ \j -> do
      thisSize <- lift $ sample antecedentsPerJustifications
      antsIdx <- lift $ if cyclic
        then intSet thisSize $ idx - 1
        else intSetExcept thisSize (totalNodes - 1) j
      let ants = map (nodes !!) antsIdx
      {-# SCC "justCalls" #-} justifyNode (show i ++ "." ++ show j) node ants
    {-# SCC "forceLabel" #-} debugNodeLabel node

