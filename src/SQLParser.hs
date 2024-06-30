{-# LANGUAGE OverloadedStrings #-}

module SQLParser (
    SQLQuery(..),
    Condition(..),
    parseSQL
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char
import System.FilePath (takeExtension)
import Debug.Trace (trace)

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

str :: String -> Parser String
str s = caseInsensitiveString s <* spaces

-- | Parse SELECT.
parseSelect :: Parser SQLQuery
parseSelect = do
  fields <- str "SELECT" *> parseSelectFields <* spaces
  table <- str "FROM" *> parseTable <* spaces
  whereClause <- optionMaybe parseWhere
  return $ SelectQuery fields table whereClause

parseUpdate :: Parser SQLQuery
parseUpdate = do
  table <- str "UPDATE" *> parseTable <* spaces
  updates <- str "SET" *> parseUpdates <* spaces
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
  table <- str "INSERT INTO" *> parseTable <* spaces
  fields <- spaces *> between (char '(') (char ')') (sepBy1 parseField (char ',')) <* spaces
  values <- str "VALUES" *> between (char '(') (char ')') (sepBy1 parseValue (char ','))
  return $ InsertQuery table fields values

parseDelete :: Parser SQLQuery
parseDelete = do
  table <- str "DELETE FROM" *> parseTable <* spaces
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

stripQuotes :: String -> String
stripQuotes = reverse . dropWhile (== '"') . reverse . dropWhile (== '"')

parseWhere :: Parser Condition
parseWhere = option (Condition "" "" "") $ do
  f <- str "WHERE" *> many1 alphaNum <* spaces
  op <- parseOperator <* spaces
  v <- many1 (noneOf " \t\n")
  return $ Condition f op (stripQuotes v)


parseOperator :: Parser String
parseOperator = try (string ">=")
            <|> try (string "<=")
            <|> string "="
            <|> string ">"
            <|> string "<"

parseField :: Parser String
parseField = do
  spaces *> many1 (noneOf ",)") <* spaces

parseValue :: Parser String
parseValue = do
  spaces *> many1 (noneOf ",)") <* spaces
