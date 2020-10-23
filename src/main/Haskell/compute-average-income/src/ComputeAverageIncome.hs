module ComputeAverageIncome
    ( computeAverageIncomeOfAllEmployees
    , createRandomStringOf80Chars
    ) where

import System.Random

data Address = Address
  { street:: String
  , postalCode:: String
  , city:: String
  , country:: String
  }

data Employee = Employee
  { firstName:: String
  , lastName:: String
  , address:: Address
  , salary:: Int
  }

-- todo: I should use the same set of chars as the other implementations
-- todo: I could use a real String instead of a list, but I do not need to
createRandomStringOf80Chars :: IO String
createRandomStringOf80Chars = do
  random <- newStdGen
  return $ take 80 $ randomRs ('a','z') random

computeAverageIncomeOfAllEmployees :: IO ()
computeAverageIncomeOfAllEmployees = putStrLn "someFunc"
