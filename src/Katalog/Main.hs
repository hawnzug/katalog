module Katalog.Main where

import Katalog.Parser (parse)
import Katalog.Preprocess (preprocess)
import Katalog.Pretty (putDatabase, putRelation, putQuery)
import Katalog.SemiNaive (run, match)
import qualified Data.Text.IO as Text.IO
import System.Environment (getArgs)
import Control.Monad (forM_)

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "Usage: katalog FILENAME"
  else do
    let filename = head args
    input <- Text.IO.readFile filename
    case parse filename input of
      Left err -> putStr err
      Right (queries, clauses) -> do
        let (db, rules) = preprocess clauses
        let newdb = run db rules
        putDatabase newdb
        putStrLn ""
        forM_ queries $ \query -> do
          putQuery query
          putStrLn ""
          putRelation $ match newdb query
          putStrLn ""
