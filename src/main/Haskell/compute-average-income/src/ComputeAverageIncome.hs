module ComputeAverageIncome
    ( computeAverageIncomeOfAllEmployees
    ) where

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

computeAverageIncomeOfAllEmployees :: IO ()
computeAverageIncomeOfAllEmployees = putStrLn "someFunc"
