import Test.QuickCheck
import Test.QuickCheck.Monadic
import ComputeAverageIncome

prop_createRandomStringOf80Chars :: Property

prop_createRandomStringOf80Chars = monadicIO $ do
  string <- run createRandomStringOf80Chars
  assert ((length string) == 80)


main :: IO ()
main = do
  quickCheck prop_createRandomStringOf80Chars
  putStrLn "Tests done"
