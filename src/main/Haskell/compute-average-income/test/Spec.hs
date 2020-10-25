import Test.QuickCheck
import Test.QuickCheck.Monadic
import ComputeAverageIncome

-- QuickCheck

prop_createRandomStringOf80Chars :: Property
prop_createRandomStringOf80Chars = monadicIO $ do
  string <- run createRandomStringOf80Chars
  assert ((length string) == 80)

prop_createRandomAddress :: Property
prop_createRandomAddress = monadicIO $ do
  address1 <- run createRandomAddress
  address2 <- run createRandomAddress
  assert (address1 /= address2)

prop_createRandomEmployee :: Property
prop_createRandomEmployee = monadicIO $ do
  employee1 <- run createRandomEmployee
  employee2 <- run createRandomEmployee
  assert (employee1 /= employee2)

prop_lookupAllEmployees :: Property
prop_lookupAllEmployees = monadicIO $ do
  let nrOfEmployees = 10
  employees <- run $ lookupAllEmployees nrOfEmployees
  assert ((length employees) == nrOfEmployees)

-- Unit Tests

-- TODO: change to unit tests, e.g. use Tasty
prop_computeAverageIncome =
  let
   address = Address "street" "code" "city" "country"
   empl1 = Employee "first" "last" address 20
   empl2 = Employee "first" "last" address 30
   empl3 = Employee "first" "last" address 40
   average = computeAverageIncome [empl1, empl2, empl3]
  in
   average == 30.0

main :: IO ()
main = do
  putStrLn "Starting tests"
  quickCheck prop_createRandomStringOf80Chars
  quickCheck prop_createRandomAddress
  quickCheck prop_createRandomEmployee
  quickCheck prop_lookupAllEmployees
  quickCheck prop_computeAverageIncome
  quickCheck prop_computeAverageIncome
  putStrLn "Tests done"
