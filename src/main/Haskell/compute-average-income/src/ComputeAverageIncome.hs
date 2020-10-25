module ComputeAverageIncome
    ( computeAverageIncomeOfAllEmployees
    , createRandomStringOf80Chars
    , createRandomAddress
    ) where

import System.Random

data Address = Address
  { street:: String
  , postalCode:: String
  , city:: String
  , country:: String
  } deriving (Eq)

data Employee = Employee
  { firstName:: String
  , lastName:: String
  , address:: Address
  , salary:: Int
  } deriving (Eq)

-- todo: I should use the same set of chars as the other implementations
-- todo: I could use a real String instead of a list, but I do not need to
createRandomStringOf80Chars :: IO String
createRandomStringOf80Chars = do
  random <- newStdGen
  return $ take 80 $ randomRs ('a','z') random

createRandomAddress :: IO Address
createRandomAddress = do
  streetV <- createRandomStringOf80Chars
  postalCodeV <- createRandomStringOf80Chars
  cityV <- createRandomStringOf80Chars
  countryV <- createRandomStringOf80Chars
  return Address
    { street = streetV
    , postalCode = postalCodeV
    , city = cityV
    , country = countryV
    }

computeAverageIncomeOfAllEmployees :: IO ()
computeAverageIncomeOfAllEmployees = putStrLn "someFunc"
