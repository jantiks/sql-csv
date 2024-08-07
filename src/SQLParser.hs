{-# LANGUAGE OverloadedStrings #-}

module SQLParser (
    SQLQuery(..),
    Condition(..),
    Expr(..),
    parseSQL
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char
import System.FilePath (takeExtension)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

data SQLQuery
  = SelectQuery [String] String (Maybe Condition)
  | UpdateQuery String [(String, Expr)] (Maybe Condition)
  | InsertQuery String [String] [String]
  | DeleteQuery String (Maybe Condition)
  deriving (Show)

data Condition = Condition Expr deriving (Show)

data Expr
  = Field String
  | IntConst Int
  | DoubleConst Double
  | StrConst String
  | BinOp String Expr Expr
  | UnOp String Expr
  deriving (Show, Eq, Ord)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString = try . mapM (\c -> char (toLower c) <|> char (toUpper c))

parseSQL :: Parser SQLQuery
parseSQL = do
  spaces
  query <- parseSelect
        <|> parseUpdate
        <|> parseInsert
        <|> parseDelete
  spaces
  return query

str :: String -> Parser String
str s = caseInsensitiveString s <* spaces

-- parse SQL commands.
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

parseUpdates :: Parser [(String, Expr)]
parseUpdates = sepBy1 parseUpdateField (char ',')

parseUpdateField :: Parser (String, Expr)
parseUpdateField = do
  f <- many1 alphaNum
  spaces
  char '='
  spaces
  e <- parseExpr
  return (f, e)


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
  let ext = takeExtension tableName
  if ext == ".csv"
    then return tableName
    else fail "Table name must end with .csv"

stripQuotes :: String -> String
stripQuotes = reverse . dropWhile (== '"') . reverse . dropWhile (== '"')

parseWhere :: Parser Condition
parseWhere = Condition <$> (str "WHERE" *> parseExpr)

lexer = Tok.makeTokenParser emptyDef

integer = Tok.integer lexer
float = Tok.float lexer
stringLiteral = Tok.stringLiteral lexer
identifier = Tok.identifier lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer

-- expression parser
parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term
  where
    table = [
              [Infix (reservedOp "*" >> return (BinOp "*")) AssocLeft
              , Infix (reservedOp "/" >> return (BinOp "/")) AssocLeft]
            , [Infix (reservedOp "+" >> return (BinOp "+")) AssocLeft
              , Infix (reservedOp "-" >> return (BinOp "-")) AssocLeft]
            , [Infix (reservedOp ">" >> return (BinOp ">")) AssocNone
              , Infix (reservedOp ">=" >> return (BinOp ">=")) AssocNone
              , Infix (reservedOp "<" >> return (BinOp "<")) AssocNone
              , Infix (reservedOp "<=" >> return (BinOp "<=")) AssocNone
              , Infix (reservedOp "=" >> return (BinOp "=")) AssocNone
              , Infix (reservedOp "!=" >> return (BinOp "!=")) AssocNone]
              ,
              [Prefix (reservedOp "not" >> return (UnOp "not")),
               Prefix (reservedOp "NOT" >> return (UnOp "not"))]
            , [Infix (reservedOp "and" >> return (BinOp "and")) AssocRight,
               Infix (reservedOp "AND" >> return (BinOp "and")) AssocRight]
            , [Infix (reservedOp "or" >> return (BinOp "or")) AssocRight,
               Infix (reservedOp "OR" >> return (BinOp "or")) AssocRight]
            ]
    term = parens parseExpr
      <|> fmap (DoubleConst . realToFrac) (try float)
      <|> fmap (IntConst . fromInteger) integer
      <|> fmap StrConst stringLiteral
      <|> fmap Field identifier

parseField :: Parser String
parseField = spaces *> many1 (noneOf ",)") <* spaces

parseStrConst :: Parser String
parseStrConst = between (char '"') (char '"') (many (noneOf "\""))

parseValue :: Parser String
parseValue = spaces *> (parseStrValue <|> parseDoubleValue <|> parseIntValue) <* spaces
  where
    parseStrValue = do
      parseStrConst
    parseDoubleValue = do
      dbl <- try float
      return (show dbl)
    parseIntValue = do
      show <$> integer