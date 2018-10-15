{-# Language OverloadedStrings #-}
{-# Language NamedFieldPuns #-}
{-# Language TupleSections #-}

module Core where

import Data.Text (Text)
import Data.Array (Array)
import Data.Vector (Vector)
import Data.Map (Map)
import qualified Data.Array as Array
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Traversable (mapAccumL)
import Data.Maybe (fromJust, catMaybes)

type Literal = Text
type Variable = Text

type Tuple = [Literal]
type Relation = Vector Tuple

type UnifyEnv = Map Variable Literal
type Parameter = Either Variable Literal

data Clause = Clause
  { clauseHead :: Predicate
  , clauseBody :: [Predicate]
  }

data Predicate = Predicate
  { predicateName :: Text
  , predicateParams :: [Parameter]
  }

type Database = Map Text Relation

parentRel :: Relation
parentRel = Vector.fromList
  [ ["alice", "bob"]
  , ["bob", "jack"]
  , ["conol", "alice"]
  ]

grandparentClause :: Clause
grandparentClause = Clause
  { clauseHead = Predicate
    { predicateName = "grandparent"
    , predicateParams = map Left ["A", "C"]
    }
  , clauseBody =
    [ Predicate
      { predicateName = "parent"
      , predicateParams = map Left ["A", "B"]
      }
    , Predicate
      { predicateName = "parent"
      , predicateParams = map Left ["B", "C"]
      }
    ]
  }
      
edb :: Database
edb = Map.singleton "parent" parentRel

checkHeadVariables :: Clause -> Bool
checkHeadVariables Clause{clauseHead, clauseBody} = all (`elem` bodyVars) headVars
  where headVars = allVars clauseHead
        bodyVars = concatMap allVars clauseBody
        allVars p = predicateParams p

query :: Database -> Clause -> Relation
query db clause = Vector.fromList $ map (go $ clauseHead clause) envs
  where
    envs :: [UnifyEnv]
    envs = multiMatch (clauseBody clause) db
    go :: Predicate -> UnifyEnv -> Tuple
    go p env = map (f env) (predicateParams p)
    f :: UnifyEnv -> Parameter -> Literal
    f env (Left v) = env Map.! v
    f _ (Right l) = l

singleMatch :: [Parameter] -> Relation -> UnifyEnv -> [UnifyEnv]
singleMatch params r env = catMaybes $ map (\tuple -> gotu params tuple env) (Vector.toList r)
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