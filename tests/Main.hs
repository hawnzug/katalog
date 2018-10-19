import Test.Tasty

import Core
import Preprocess
import SemiNaive

main = defaultMain tests

tests :: TestTree
tests = testGroup "Inner tests"
  [ coreTests
  , preprocessTests
  , semiNaiveTests
  ]
