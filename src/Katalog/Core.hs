{-# Language OverloadedStrings #-}
{-# Language TupleSections #-}

module Katalog.Core where

import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable (mapAccumL)
import Data.Maybe (catMaybes)

type Literal = Text
type Variable = Text

type Tuple = [Literal]
type Relation = Set Tuple

type UnifyEnv = Map Variable Literal
type Parameter = Either Variable Literal

data Clause = Clause
  { clauseHead :: Predicate
  , clauseBody :: [Predicate]
  } deriving (Show)

data Predicate = Predicate
  { predicateName :: Text
  , predicateParams :: [Parameter]
  } deriving (Show)

type Database = Map Text Relation

query :: Database -> Clause -> Relation
query db clause = Set.fromList $ map (go $ clauseHead clause) envs
  where
    envs :: [UnifyEnv]
    envs = multiMatch (clauseBody clause) db
    go :: Predicate -> UnifyEnv -> Tuple
    go p env = map (f env) (predicateParams p)
    f :: UnifyEnv -> Parameter -> Literal
    f env (Left v) = env Map.! v
    f _ (Right l) = l

singleMatch :: [Parameter] -> Relation -> UnifyEnv -> [UnifyEnv]
singleMatch params r env = catMaybes $ map (\tuple -> gotu params tuple env) (Set.toList r)
  where
    go :: Parameter -> Literal -> UnifyEnv -> Maybe UnifyEnv
    go (Right l) lit env = if l == lit then Just env else Nothing
    go (Left v) lit env = case Map.lookup v env of
      Just l -> if l == lit then Just env else Nothing
      Nothing -> Just $ Map.insert v lit env
    gotu :: [Parameter] -> Tuple -> UnifyEnv -> Maybe UnifyEnv
    gotu [] [] env = Just env
    gotu (p:ps) (l:ls) env = do
      env' <- go p l env
      gotu ps ls env'
    gotu _ _ _ = error "Predicate and tuple's arity should be the same"

multiMatch :: [Predicate] -> Database -> [UnifyEnv]
multiMatch ps db = fst $ mapAccumL single [Map.empty] ps
  where
    getRelation p = db Map.! (predicateName p)
    single :: [UnifyEnv] -> Predicate -> ([UnifyEnv], ())
    single envs p = (concatMap (singleMatch (predicateParams p) (getRelation p)) envs, ())

databaseInsert :: Text -> Relation -> Database -> Database
databaseInsert name rel db = Map.insertWith Set.union name rel db
