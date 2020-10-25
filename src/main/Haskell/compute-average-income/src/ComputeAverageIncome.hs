module ComputeAverageIncome
    ( computeAverageIncomeOfAllEmployees
    , createRandomStringOf80Chars
    , createRandomAddress
    , createRandomEmployee
    , lookupAllEmployees
    ) where

import System.Random
import Control.Monad

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

createRandomEmployee :: IO Employee
createRandomEmployee = do
  firstNameV <- createRandomStringOf80Chars
  lastNameV <- createRandomStringOf80Chars
  addressV <- createRandomAddress
  let salaryV = 1000
  return Employee
    { firstName = firstNameV
    , lastName = lastNameV
    , address = addressV
    , salary = salaryV
}

lookupAllEmployees :: Int -> IO [Employee]
lookupAllEmployees numberOfAllEmployees =
  replicateM numberOfAllEmployees createRandomEmployee


computeAverageIncomeOfAllEmployees :: IO ()
computeAverageIncomeOfAllEmployees = putStrLn "someFunc"
