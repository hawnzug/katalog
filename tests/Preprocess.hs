{-# Language OverloadedStrings #-}
module Preprocess (preprocessTests) where

import Katalog.Core
import Katalog.Preprocess (checkHeadVariables, groupByPredicate)

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Utils

preprocessTests :: TestTree
preprocessTests = testGroup "Preprocess" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit Test"
  [ unsafe "all(A) :- parent(X, Y)."
  , unsafe "grandparent(X, Y) :- parent(X, Z)."
  , unsafe "single(A)."
  ]

unsafe :: Text.Text -> TestTree
unsafe input = testCase (Text.unpack input) $ do
  clause <- parseClause input
  not (checkHeadVariables clause) @? "This clause should be unsafe"

propertyTests :: TestTree
propertyTests = testGroup "QuickCheck"
  [ testProperty "Group clauses by predicate" groupbyQuickCheck ]

instance Arbitrary Clause where
  arbitrary = oneof [genFact, genRule]

groupbyQuickCheck :: [Clause] -> Bool
groupbyQuickCheck clauses = allFact && allRule && sameSet
  && consistent factsMap && consistent rulesMap
  where
    (factsMap, rulesMap) = groupByPredicate clauses
    allFact = all (all (null . clauseBody)) factsMap
    allRule = all (all (not . null . clauseBody)) rulesMap
    sameSet = Set.fromList clauses == Set.fromList after
      where after = concat $ Map.elems factsMap ++ Map.elems rulesMap
    consistent m = all great $ Map.mapWithKey (,) m
      where great (name, clauses) = all ((==) name . predicateName . clauseHead) clauses
