{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SQLParser (SQLQuery(..), parseSQL, Condition(..), Expr(Field))
import qualified CSVFilter as CF
import Text.Parsec (parse)
import System.Environment (getArgs)
import qualified Data.Text as T
import System.Exit (exitFailure)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  if null args
     then do
        showUsage
        exitFailure
  else
    case parse parseSQL "" (unwords args) of
        Left err  -> print err
        Right sql -> executeSQL sql

executeSQL :: SQLQuery -> IO ()
executeSQL (SelectQuery fields table whereClause) = do
    let fieldNames = if fields == ["*"] then [] else map T.pack fields
    CF.runSQLQuery table fieldNames (fromMaybe (Condition (Field "")) whereClause)

executeSQL (DeleteQuery table whereClause) = do
    case whereClause of
        Nothing -> putStrLn "DELETE Query requires a WHERE clause"
        Just cond ->
            CF.runDeleteQuery table cond

executeSQL (UpdateQuery table updates whereClause) = do
    CF.runUpdateQuery table (map (\(fld, val) -> (T.pack fld, val)) updates) (fromMaybe (Condition (Field "")) whereClause)

executeSQL (InsertQuery table fields values) = do
    CF.runInsertQuery table (map T.pack fields) (map T.pack values)


showUsage :: IO ()
showUsage = do
    putStrLn "Usage: sql-csv-exe '<Command>' '<Filepath>' WHERE '<Condition>'"