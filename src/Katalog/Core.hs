{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Katalog.Core where

import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Control.Monad (foldM)

type Literal = Text
type Variable = Text

type Tuple = [Literal]
type Relation = Set Tuple

type UnifyEnv = Map Variable Literal
type Parameter = Either Variable Literal

data Clause = Clause
  { clauseHead :: Predicate
  , clauseBody :: [Predicate]
  } deriving (Show, Eq, Ord)

data Predicate = Predicate
  { predicateName :: Text
  , predicateParams :: [Parameter]
  } deriving (Show, Eq, Ord)

type Database = Map Text Relation

query :: Database -> Clause -> Relation
query db (Clause (Predicate _ params) body) =
  Set.fromList $ map (go params) (multiMatch body db)
  where go params env = map (either (env Map.!) id) params

singleMatch :: [Parameter] -> Relation -> UnifyEnv -> [UnifyEnv]
singleMatch params r env = catMaybes $ map (goto env params) (Set.toList r)
  where
    go :: UnifyEnv -> (Parameter, Literal) -> Maybe UnifyEnv
    go env (Right l, lit) = if l == lit then Just env else Nothing
    go env (Left v, lit)  = case Map.lookup v env of
      Just l -> if l == lit then Just env else Nothing
      Nothing -> Just $ Map.insert v lit env
    goto :: UnifyEnv -> [Parameter] -> Tuple -> Maybe UnifyEnv
    goto env ps ls = foldM go env (zip ps ls)

multiMatch :: [Predicate] -> Database -> [UnifyEnv]
multiMatch ps db = foldM single Map.empty ps
  where
    getRelation p = db Map.! (predicateName p)
    single :: UnifyEnv -> Predicate -> [UnifyEnv]
    single env p = singleMatch (predicateParams p) (getRelation p) env

databaseInsert :: Text -> Relation -> Database -> Database
databaseInsert name rel db = Map.insertWith Set.union name rel db
