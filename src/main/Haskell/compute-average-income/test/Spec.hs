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

main :: IO ()
main = do
  putStrLn "Starting tests"
  quickCheck prop_createRandomStringOf80Chars
  quickCheck prop_createRandomAddress
  putStrLn "Tests done"
