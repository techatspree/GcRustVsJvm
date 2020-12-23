module Main where

import ComputeAverageIncomeStrict

main :: IO ()
main = do
  income <- computeAverageIncomeOfAllEmployeesStrict 1
  putStrLn ("Result of computation: " ++ (show income))
