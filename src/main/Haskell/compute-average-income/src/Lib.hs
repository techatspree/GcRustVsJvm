module ComputeAverageIncome
    ( someFunc
    ) where

data Address
  { street:: String
  , postalCode:: String
  , city:: String
  , country: String
  }

data Employee
  { firstName:: String
  , lastName:: String
  , address:: Address
  , salary: Int
  }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
