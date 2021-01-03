module Main where

import Criterion.Main
import ComputeAverageIncomeStrict

main :: IO ()

{- profile just one execution
main = do
  income <- computeAverageIncomeOfAllEmployeesStrict 1
  putStrLn ("Result of computation: " ++ (show income))
-}

{- profile the strict benchmark -}
main = defaultMain [
        bgroup "compute average income strict"
          [ bench "10^3"  $ nfIO (computeAverageIncomeOfAllEmployeesStrict 1000)
          , bench "10^4"  $ nfIO (computeAverageIncomeOfAllEmployeesStrict 10000)
          , bench "10^5"  $ nfIO (computeAverageIncomeOfAllEmployeesStrict 100000)
          , bench "10^6"  $ nfIO (computeAverageIncomeOfAllEmployeesStrict 1000000)
          ]
      ]

