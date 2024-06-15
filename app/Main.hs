module Main (main) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (many1, optionMaybe)
import Control.Monad (void)
import Data.Char
import Data.List
import System.Environment (getArgs)

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
  void $ caseInsensitiveString "SELECT"
  void spaces
  fields <- parseSelectFields
  void spaces
  void $ caseInsensitiveString "FROM"
  void spaces
  table <- parseTable
  void spaces
  whereClause <- optionMaybe parseWhere
  return $ SelectQuery fields table whereClause

parseUpdate :: Parser SQLQuery
parseUpdate = do
  void $ caseInsensitiveString "UPDATE"
  void spaces
  table <- parseTable
  void spaces
  void $ caseInsensitiveString "SET"
  void spaces
  updates <- parseUpdates
  void spaces
  whereClause <- optionMaybe parseWhere
  return $ UpdateQuery table updates whereClause

parseUpdates :: Parser [(String, String)]
parseUpdates = sepBy1 parseUpdateField (char ',')

parseUpdateField :: Parser (String, String)
parseUpdateField = do
  f <- many1 alphaNum
  void spaces
  void $ char '='
  void spaces
  v <- many1 (noneOf ",")
  return (f, v)


-- | Parse INSERT.
parseInsert :: Parser SQLQuery
parseInsert = do
  void $ caseInsensitiveString "INSERT INTO"
  void spaces
  table <- parseTable
  void spaces
  fields <- between (char '(') (char ')') (sepBy (many1 alphaNum) (char ','))
  void spaces
  void $ caseInsensitiveString "VALUES"
  void spaces
  values <- between (char '(') (char ')') (sepBy (many1 (noneOf ",)")) (char ','))
  return $ InsertQuery table fields values

-- | Parse a DELETE query.
parseDelete :: Parser SQLQuery
parseDelete = do
  void $ caseInsensitiveString "DELETE FROM"
  void spaces
  table <- parseTable
  void spaces
  whereClause <- optionMaybe parseWhere
  return $ DeleteQuery table whereClause

-- | Parse the fields in the SELECT statement.
parseSelectFields :: Parser [String]
parseSelectFields = do
  sepBy1 (many1 (alphaNum <|> char '*')) (char ',')

-- | Parse csv file.
parseTable :: Parser String
parseTable = do
  tableName <- many1 (alphaNum <|> char '.')
  if ".csv" `isSuffixOf` tableName
    then return tableName
    else fail "Table name must end with .csv"

-- | Parse WHERE.
parseWhere :: Parser Condition
parseWhere = do
  void $ caseInsensitiveString "WHERE"
  void consumeSpaces
  f <- many1 alphaNum
  void consumeSpaces
  op <- parseOperator
  void consumeSpaces
  v <- many1 (noneOf " \t\n")
  return $ Condition f op v

parseOperator :: Parser String
parseOperator = try (string ">=")
            <|> try (string "<=")
            <|> try (string "<>")
            <|> string "="
            <|> string ">"
            <|> string "<"

consumeSpaces :: Parser ()
consumeSpaces = skipMany1 (satisfy isSpace)


main :: IO ()
main = do
  args <- getArgs
  let input = intercalate " " args
  case parse parseSQL "" input of
    Left err  -> print err
    Right sql -> do
      
      print sql
      
      case sql of
        SelectQuery fields table whereClause -> do
          putStrLn "Fields:"
          print fields
          putStrLn "Table:"
          print table
          putStrLn "Where Clause:"
          case whereClause of
            Nothing -> putStrLn "No where clause"
            Just (Condition field op value) -> do
              putStrLn "Field:"
              print field
              putStrLn "Operator:"
              print op
              putStrLn "Value:"
              print value
        UpdateQuery table updates whereClause -> do
          putStrLn "Table:"
          print table
          putStrLn "Updates:"
          print updates
          putStrLn "Where Clause:"
          case whereClause of
            Nothing -> putStrLn "No where clause"
            Just (Condition field op value) -> do
              putStrLn "Field:"
              print field
              putStrLn "Operator:"
              print op
              putStrLn "Value:"
              print value
        InsertQuery table fields values -> do
          putStrLn "Table:"
          print table
          putStrLn "Fields:"
          print fields
          putStrLn "Values:"
          print values
        DeleteQuery table whereClause -> do
          putStrLn "Table:"
          print table
          putStrLn "Where Clause:"
          case whereClause of
            Nothing -> putStrLn "No where clause"
            Just (Condition field op value) -> do
              putStrLn "Field:"
              print field
              putStrLn "Operator:"
              print op
              putStrLn "Value:"
              print value