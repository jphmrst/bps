module Main where

import ATMSTrun
import JTMSrun
import Control.Monad.Random

main :: IO ()
main = do
  runATMS1
  -- runJTMS1
  return ()

intSet :: (RandomGen g) => Int -> Int -> Rand g [Int]
intSet 0 _ = return []
intSet n m = do
  x <- getRandomR (0, m)
  xs <- intSet (n - 1) m
  return $ x : xs

intSetExcept :: (RandomGen g) => Int -> Int -> Int -> Rand g [Int]
intSetExcept 0 _ _ = return []
intSetExcept n m d = do
  x <- getRandomR (0, m)
  if (x == d) then intSetExcept n m d else do
    xs <- intSetExcept (n - 1) m d
    return $ x : xs

data IntRange = IntRange { lo :: Int, hi :: Int }

sample :: (RandomGen g) => IntRange -> Rand g Int
sample (IntRange lo hi) = getRandomR (lo, hi)

makeATMS ::
  (RandomGen g) =>
    IntRange -> IntRange -> Double -> IntRange -> IntRange -> Bool -> Rand g Int
makeATMS assumptionsRange nonassumptionsRange contradictionChance
         justificationsPerConclusion antecedentsPerJustifications cyclic = do
  error "TODO"
