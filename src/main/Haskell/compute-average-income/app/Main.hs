module Main where

import Criterion.Main
import ComputeAverageIncome

main :: IO ()
main = defaultMain [
         bgroup "compute average income"
          [ bench "10^3"  $ nfIO (computeAverageIncomeOfAllEmployees 1000)
          , bench "10^4"  $ nfIO (computeAverageIncomeOfAllEmployees 10000)
          , bench "10^5"  $ nfIO (computeAverageIncomeOfAllEmployees 100000)
          , bench "10^6"  $ nfIO (computeAverageIncomeOfAllEmployees 1000000)
          ]
      ]


