{-# Language OverloadedStrings #-}
module SemiNaive (semiNaiveTests) where

import Katalog.Core
import Katalog.SemiNaive (substClause)

import Data.List (findIndex)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Utils

semiNaiveTests :: TestTree
semiNaiveTests = testGroup "SemiNaive" [propertyTests]

propertyTests :: TestTree
propertyTests = testGroup "QuickCheck"
  [ testProperty "Substitute clause" substQuickCheck ]

newtype RuleClause = RuleClause Clause deriving (Show)

instance Arbitrary RuleClause where
  arbitrary = RuleClause <$> genRule

substQuickCheck :: RuleClause -> Bool
substQuickCheck (RuleClause clause) = True
