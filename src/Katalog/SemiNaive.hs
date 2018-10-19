{-# Language OverloadedStrings #-}
module Katalog.SemiNaive where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Map (Map)
import Data.Either (lefts)

import Katalog.Core

type TwoDB = (Database, Database)

eval :: Database -> [Clause] -> Relation
eval db = Set.unions . map (query db)

substClause :: Clause -> [Clause]
substClause (Clause h body) = map (Clause h) (go body)
  where
    go :: [Predicate] -> [[Predicate]]
    go [] = []
    go (p:ps) = ((subst p):ps) : (map (p:) (go ps))
    subst (Predicate name params) = Predicate (Text.cons '_' name) params

deltaEval :: TwoDB -> [Clause] -> Relation
deltaEval (db, delta) clauses = eval newdb newclauses
  where
    newclauses = concatMap substClause clauses
    newdb = Set.foldl f db ps
    ps = Set.fromList $ concatMap (map predicateName . clauseBody) clauses
    f d n = Map.insert (Text.cons '_' n) (delta Map.! n) d

iter :: TwoDB -> Map Text [Clause] -> TwoDB
iter (db, delta) rules = (db', delta')
  where
    db' = Map.unionWith Set.union db delta
    delta' = Map.unionWith Set.difference (Map.map (deltaEval (db, delta)) rules) db'

iters :: TwoDB -> Map Text [Clause] -> TwoDB
iters d@(db, delta) clauses = if all Set.null delta then d else iters (iter d clauses) clauses

run :: Database -> Map Text [Clause] -> Database
run db rules = fst $ iters (Map.unionWith Set.union db delta, delta) rules
  where delta = Map.map (eval db) rules

match :: Database -> [Predicate] -> Relation
match db ps = query db dummyClause
  where
    dummyClause = Clause dummyHead ps
    dummyHead = Predicate "__dummy__" $ map Left (concatMap allVars ps)
    allVars = lefts . predicateParams
