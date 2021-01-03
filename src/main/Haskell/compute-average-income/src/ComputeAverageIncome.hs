module ComputeAverageIncome
    ( computeAverageIncomeOfAllEmployees
    , createRandomStringOf80Chars
    , createRandomAddress
    , createRandomEmployee
    , lookupAllEmployees
    , computeAverageIncome
    , Address( Address )
    , Employee( Employee)
    ) where

import System.Random
import Control.Monad

data Address = Address
  { street:: String
  , postalCode:: String
  , city:: String
  , country:: String
  } deriving (Eq, Show)

data Employee = Employee
  { firstName:: String
  , lastName:: String
  , address:: Address
  , salary:: Int
  } deriving (Eq, Show)

chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
nrOfChars = length chars

createRandomStringOf80Chars :: IO String
createRandomStringOf80Chars = do
  random <- newStdGen
  return $
    take 80
      $ map (\i -> chars !! i)
      $ randomRs ( 0,nrOfChars -1) random

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

computeAverageIncome :: [Employee] -> Float
computeAverageIncome employees =
  let (nrEmployees, sumOfAllSalaries) =
        foldr (\ employee (counter, sum)  -> (counter + 1, sum + (salary employee)))
              (0,0) employees
  in
    (fromIntegral sumOfAllSalaries) / (fromIntegral nrEmployees)

computeAverageIncomeOfAllEmployees :: Int -> IO (Float)
computeAverageIncomeOfAllEmployees numberOfAllEmployees = do
  employees <- lookupAllEmployees numberOfAllEmployees
  return $ computeAverageIncome employees