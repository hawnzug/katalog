{-# Language OverloadedStrings #-}
module Core (coreTests) where

import Katalog.Core

import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Tasty
import Test.Tasty.HUnit

import Utils (parseClause)

parent :: Relation
parent = Set.fromList
  [ ["a", "b"]
  , ["b", "c"]
  , ["c", "d"]
  , ["d", "e"]
  ]

ancestor :: Relation
ancestor = Set.fromList
  [ ["a", "c"]
  , ["b", "d"]
  , ["c", "e"]
  ]

databaseExample :: Database
databaseExample = Map.fromList
  [ ("parent",   parent)
  , ("ancestor", parent)
  ]

coreTests :: TestTree
coreTests = testGroup "Core"
  [ should "all(X, Y) :- parent(X, Y)." parent
  , should "grandparent(X, Z) :- parent(X, Y), parent(Y, Z)." ancestor
  , should "ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)." ancestor
  ]

should :: Text.Text -> Relation -> TestTree
should input expected = testCase (Text.unpack input) $ do
  clause <- parseClause input
  expected @=? query databaseExample clause
