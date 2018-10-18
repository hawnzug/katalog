module Main where

import Parser (parse)
import Preprocess (preprocess)
import SemiNaive (run)
import qualified Data.Text.IO as Text.IO
import System.Environment (getArgs)
import Pretty (putDatabase)

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
      Right clauses -> do
        let (db, rules) = preprocess clauses
        putDatabase $ run db rules