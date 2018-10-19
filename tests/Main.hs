import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
  [ testCase "List length" $ length [1, 2, 3] @?= 3 ]
