{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}
module Katalog.SemiNaive where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set)
import Data.Either (lefts)

import Katalog.Core

type DBDB = (Database, Database)
type DBDBDB = (Database, Database, Database)

eval :: Database -> [Clause] -> Relation
eval db = Set.unions . map (query db)

substClause :: Clause -> [Clause]
substClause (Clause h body) = map (Clause h) (go body)
  where
    go :: [Term] -> [[Term]]
    go [] = []
    go (t:ts) = case t of
      TermPre p -> ((TermPre $ subst '_' p):ts) :
                   (map ((TermPre $ subst '?' p):) (go ts))
      _ -> map (t:) (go ts)
    subst c (Predicate name params) = Predicate (Text.cons c name) params

deltaEval :: DBDBDB -> (Set Text, [Clause]) -> Relation
deltaEval (db, delta, db') (ps, clauses) = eval db2 clauses
  where
    db1 = Set.foldl' (f '_' delta) db ps
    db2 = Set.foldl' (f '?' db') db1 ps
    f c td d n = Map.insert (Text.cons c n) (td Map.! n) d

iter :: DBDB -> Map Text (Set Text, [Clause]) -> DBDB
iter (db, delta) rules = (db', delta')
  where
    db' = Map.unionWith Set.union db delta
    delta' = Map.unionWith Set.difference (Map.map (deltaEval (db, delta, db')) rules) db'

iters :: DBDB -> Map Text (Set Text, [Clause]) -> DBDB
iters d@(db, delta) clauses = if all Set.null delta then d else iters (iter d clauses) clauses

run :: Database -> Map Text ([Clause]) -> Database
run db rules = fst $ iters (Map.unionWith Set.union db delta, delta) deltaRules
  where delta  = Map.map (eval db) rules
        deltaRules = Map.map f rules
          where f clauses = (g clauses, concatMap substClause clauses)
                g clauses = Set.fromList $
                            concatMap (map name . clauseBody) clauses
                name = predicateName . \case
                  TermPre p -> p
                  TermNeg p -> p
                  

match :: Database -> [Term] -> Relation
match db ts = query db dummyClause
  where
    dummyClause = Clause dummyHead ts
    dummyHead = Predicate "__dummy__" $ map Left (concatMap allVars ts)
    allVars = lefts . predicateParams . \case
      TermPre p -> p
      TermNeg p -> p
