import Test.QuickCheck
import Test.QuickCheck.Monadic
import ComputeAverageIncome

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



main :: IO ()
main = do
  putStrLn "Starting tests"
  quickCheck prop_createRandomStringOf80Chars
  quickCheck prop_createRandomAddress
  quickCheck prop_createRandomEmployee
  putStrLn "Tests done"
