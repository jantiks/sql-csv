{-# LANGUAGE OverloadedStrings #-}

module SQLParser (
    SQLQuery(..),
    Condition(..),
    parseSQL
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (many1, optionMaybe, sepBy1, between)
import Control.Monad (void)
import Data.Char
import System.FilePath (takeExtension)
import Debug.Trace (trace, traceShowId)

data SQLQuery
  = SelectQuery [String] String (Maybe Condition)
  | UpdateQuery String [(String, String)] (Maybe Condition)
  | InsertQuery String [String] [String]
  | DeleteQuery String (Maybe Condition)
  deriving (Show)

data Condition = Condition
  { field    :: String
  , operator :: String
  , value    :: String
  } deriving (Show)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString = try . mapM (\c -> char (toLower c) <|> char (toUpper c))

parseSQL :: Parser SQLQuery
parseSQL = do
  spaces
  query <- try parseSelect
        <|> try parseUpdate
        <|> try parseInsert
        <|> try parseDelete
  spaces
  return query

-- | Parse SELECT.
parseSelect :: Parser SQLQuery
parseSelect = do
  caseInsensitiveString "SELECT"
  spaces
  fields <- parseSelectFields
  spaces
  caseInsensitiveString "FROM"
  spaces
  table <- parseTable
  spaces
  whereClause <- optionMaybe parseWhere
  return $ SelectQuery fields table whereClause

parseUpdate :: Parser SQLQuery
parseUpdate = do
  caseInsensitiveString "UPDATE"
  spaces
  table <- parseTable
  spaces
  caseInsensitiveString "SET"
  spaces
  updates <- parseUpdates
  spaces
  whereClause <- optionMaybe parseWhere
  return $ UpdateQuery table updates whereClause

parseUpdates :: Parser [(String, String)]
parseUpdates = sepBy1 parseUpdateField (char ',')

parseUpdateField :: Parser (String, String)
parseUpdateField = do
  f <- many1 alphaNum
  spaces
  char '='
  spaces
  v <- many1 (noneOf ", \t\n")
  return (f, v)

parseInsert :: Parser SQLQuery
parseInsert = do
  caseInsensitiveString "INSERT INTO"
  void spaces
  table <- parseTable
  spaces
  fields <- between (char '(') (char ')') (sepBy1 parseField (char ','))
  trace ("Parsed fields: " ++ show fields) $ return ()
  spaces
  caseInsensitiveString "VALUES"
  spaces
  values <- between (char '(') (char ')') (sepBy1 parseValue (char ','))
  trace ("Parsed values: " ++ show values) $ return ()
  return $ InsertQuery table fields values

parseDelete :: Parser SQLQuery
parseDelete = do
  caseInsensitiveString "DELETE FROM"
  spaces
  table <- parseTable
  spaces
  whereClause <- optionMaybe parseWhere
  return $ DeleteQuery table whereClause

parseSelectFields :: Parser [String]
parseSelectFields = sepBy1 (many1 (alphaNum <|> char '*')) (char ',')

parseTable :: Parser String
parseTable = do
  tableName <- many1 (noneOf " \t\n")
  trace ("Table name: " ++ tableName) $ do
    let ext = takeExtension tableName
    if ext == ".csv"
      then return tableName
      else fail "Table name must end with .csv"

parseWhere :: Parser Condition
parseWhere = option (Condition "" "" "") $ do
  caseInsensitiveString "WHERE"
  void spaces
  f <- many1 alphaNum
  spaces
  op <- parseOperator
  spaces
  v <- many1 (noneOf " \t\n")
  return $ Condition f op v

parseOperator :: Parser String
parseOperator = try (string ">=")
            <|> try (string "<=")
            <|> string "="
            <|> string ">"
            <|> string "<"
 
parseField :: Parser String
parseField = do
  spaces
  f <- many1 (noneOf ",)")
  spaces
  return f

parseValue :: Parser String
parseValue = do
  spaces
  v <- many1 (noneOf ",)")
  spaces
  return v
