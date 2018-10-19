{-# Language OverloadedStrings #-}
module Utils where

import Katalog.Core
import Katalog.Parser (parse)

import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty.HUnit (assertFailure)
import Test.QuickCheck

parseClause :: Text -> IO Clause
parseClause input = case parse "<Test>" input of
  Right (_, clause:_) -> return clause
  Left err            -> assertFailure err
  _                   -> assertFailure "Parse error: not a single clause"

parseClauses :: Text -> IO [Clause]
parseClauses input = case parse "<Test>" input of
  Right (_, clauses) -> return clauses
  Left err           -> assertFailure err

genPredicateName :: Gen Text
genPredicateName = elements ["up", "down", "parent", "ancestor", "path", "same"]

genVariable :: Gen Text
genVariable = Text.singleton <$> choose ('A', 'Z')

genLiteral :: Gen Text
genLiteral = Text.singleton <$> choose ('a', 'z')

genFactPredicate :: Gen Predicate
genFactPredicate = Predicate <$> genPredicateName <*> (map Right <$> listOf genLiteral)

genRulePredicate :: Gen Predicate
genRulePredicate = Predicate <$> genPredicateName <*> (map Left <$> listOf genVariable)

genFact :: Gen Clause
genFact = Clause <$> genFactPredicate <*> pure []

genRule :: Gen Clause
genRule = Clause <$> genRulePredicate <*>
  listOf (oneof [genRulePredicate, genFactPredicate])
