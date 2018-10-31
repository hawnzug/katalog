{-# Language OverloadedStrings #-}
module Katalog.Parser where

import Katalog.Core (Parameter, Predicate(..), Clause(..), Term(..))
import Control.Monad.Combinators
import Data.Text (Text)
import Data.Char (isLower)
import Data.Either (partitionEithers)
import qualified Data.Text as Text
import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Char as Mega.Char
import qualified Text.Megaparsec.Error as Mega.Error
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Data.Void (Void)

type Parser = Mega.Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = Lexer.space Mega.Char.space1 lineCmnt blockCmnt
  where
    lineCmnt  = Lexer.skipLineComment "//"
    blockCmnt = Lexer.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = Lexer.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

literal :: Parser Text
literal = lexeme $ Text.pack <$> name
  where name = (:) <$> Mega.Char.lowerChar <*> many Mega.Char.alphaNumChar

variable :: Parser Text
variable = lexeme $ Text.pack <$> name
  where name = (:) <$> Mega.Char.upperChar <*> many Mega.Char.alphaNumChar

number :: Parser Text
number = lexeme $ Text.pack <$> some Mega.Char.digitChar

param :: Parser Parameter
param = (Right <$> (number <|> literal)) <|> (Left <$> variable)

predicate :: Parser Predicate
predicate = do
  n <- literal
  ps <- parens (param `sepBy1` symbol ",")
  return $ Predicate n ps

term :: Parser Term
term = (TermNeg <$> (Mega.try (symbol "not") *> predicate)) <|> (TermPre <$> predicate)

clause :: Parser Clause
clause = do
  hd <- predicate
  tl <- (Mega.try (symbol ":-") >> term `sepBy1` symbol ",") <|> return []
  symbol "."
  return $ Clause hd tl

type Query = [Term]
query :: Parser Query
query = term `sepBy1` symbol "," <* symbol "?"

line :: Parser (Either Query Clause)
line = (Left <$> Mega.try query) <|> (Right <$> clause)

wholeParser :: Parser [Either Query Clause]
wholeParser = between spaceConsumer Mega.eof (many line)

parse :: String -> Text -> Either String ([Query], [Clause])
parse filename input = case Mega.parse wholeParser filename input of
  Left err -> Left (Mega.Error.parseErrorPretty err)
  Right cs -> Right $ partitionEithers cs
