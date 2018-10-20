module Katalog.Main where

import Katalog.Parser (parse)
import Katalog.Preprocess (preprocess)
import Katalog.Pretty (renderRelation, renderQuery)
import Katalog.SemiNaive (run, match)
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import System.Environment (getArgs)

execute :: FilePath -> IO [Text.Lazy.Text]
execute filename = do
  input <- Text.IO.readFile filename
  case parse filename input of
    Left err -> return $ [Text.Lazy.pack err]
    Right (queries, clauses) -> do
      let (db, rules) = preprocess clauses
      let newdb = run db rules
      return $ flip concatMap queries $ \query ->
        [renderQuery query, renderRelation $ match newdb query]

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "Usage: katalog FILENAME"
  else execute (head args) >>= mapM_ Text.Lazy.IO.putStrLn
