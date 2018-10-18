{-# Language NamedFieldPuns #-}
module Preprocess where

import Core

import Data.Either (lefts)
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

checkHeadVariables :: Clause -> Bool
checkHeadVariables Clause{clauseHead, clauseBody} = null headVars || all (`elem` bodyVars) headVars
  where headVars = allVars clauseHead
        bodyVars = concatMap allVars clauseBody
        allVars p = lefts $ predicateParams p

-- all (null . clauseBody) clauses == True
-- all checkHeadVariables clauses == True
storeFacts :: Database -> Text -> [Clause] -> Database
storeFacts db name clauses = databaseInsert name (Set.fromList facts) db
  where
    facts :: [Tuple]
    facts = map (map unwrap . predicateParams . clauseHead) clauses
    unwrap :: Parameter -> Literal
    unwrap (Left _)  = error "cannot happen if head variables are checked"
    unwrap (Right l) = l

type Fact = Clause
type Rule = Clause

groupByPredicate :: [Clause] -> (Map Text [Fact], Map Text [Rule])
groupByPredicate = foldl insert (Map.empty, Map.empty)
  where
    insert (mf, mr) c = if isFact then (modify mf, mr) else (mf, modify mr)
      where
        modify = Map.alter (Just . maybe [c] (c:)) headPred
        isFact = null (clauseBody c)
        headPred = predicateName $ clauseHead c

preprocess :: [Clause] -> (Database, Map Text [Rule])
preprocess clauses = if not (all checkHeadVariables clauses) then error "Variables in head are not bounded" else
  let (mf, mr) = groupByPredicate clauses
      allnames = Map.keys mf ++ Map.keys mr
      initdb = Map.fromSet (const Set.empty) (Set.fromList allnames)
      db = Map.foldlWithKey storeFacts initdb mf
      mr' = Map.union mr (Map.map (const []) mf)
  in (db, mr')