{-# Language OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (replaceExtension)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as Text.Lazy
import Data.List (intersperse)

import Core
import Preprocess
import SemiNaive
import Katalog.Main (execute)

main = do
  goldens <- goldenTests
  defaultMain $ testGroup "Tests" [goldens, tests]

tests :: TestTree
tests = testGroup "Inner tests"
  [ coreTests
  , preprocessTests
  , semiNaiveTests
  ]

goldenTests :: IO TestTree
goldenTests = do
  examples <- findByExtension [".dl"] "examples"
  return $ testGroup "Golden tests"
    [ goldenVsString example output result
    | example <- examples
    , let output = replaceExtension example ".out"
    , let result = encodeUtf8 . Text.Lazy.concat . intersperse "\n" <$> execute example
    ]
