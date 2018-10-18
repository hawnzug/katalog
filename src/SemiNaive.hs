module SemiNaive where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Map (Map)

import Core

type TwoDB = (Database, Database)

eval :: Database -> [Clause] -> Relation
eval db = Set.unions . map (query db)

deltaEval :: TwoDB -> Map Text [Clause] -> Database
deltaEval (db, deltadb) = Map.map single
  where
    single :: [Clause] -> Relation
    single clauses = Set.unions $ map (substEval clauses) (Map.assocs deltadb)
    substEval :: [Clause] -> (Text, Relation) -> Relation
    substEval clauses (k, v) = eval (Map.insert k v db) clauses

iter :: TwoDB -> Map Text [Clause] -> TwoDB
iter (db, delta) clauses = (db', delta')
  where
    db' = Map.unionWith Set.union db delta
    delta' = Map.unionWith Set.difference (deltaEval (db, delta) clauses) db'

iters :: TwoDB -> Map Text [Clause] -> TwoDB
iters d@(_, delta) clauses = if all Set.null delta then d else iters (iter d clauses) clauses

run :: Database -> Map Text [Clause] -> Database
run db rules = fst $ iters (db, Map.map (eval db) rules) rules