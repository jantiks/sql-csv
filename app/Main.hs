{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SQLParser (SQLQuery(..), parseSQL, Condition(..), Expr(Field))
import qualified CSVFilter as CF
import Text.Parsec (parse)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Text as T
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  if null args
     then do 
        showUsage
        exitFailure
  else
    case parse parseSQL "" (intercalate " " args) of
        Left err  -> print err
        Right sql -> executeSQL sql

executeSQL :: SQLQuery -> IO ()
executeSQL (SelectQuery fields table whereClause) = do
    let fieldNames = if fields == ["*"] then [] else map T.pack fields
    case whereClause of
        Just cond ->
            CF.runSQLQuery table fieldNames cond
        Nothing ->
            CF.runSQLQuery table fieldNames (Condition $ Field "")
    -- print records


executeSQL (DeleteQuery table whereClause) = do
    case whereClause of
        Nothing -> putStrLn "DELETE Query requires a WHERE clause"
        Just cond -> 
            CF.runDeleteQuery table cond

executeSQL (UpdateQuery table updates whereClause) = do
    case whereClause of
        Nothing -> putStrLn "UPDATE Query requires a WHERE clause"
        Just cond -> 
            CF.runUpdateQuery table 
            (map (\(fld, val) -> (T.pack fld, T.pack val)) updates) 
            (cond)
        
executeSQL (InsertQuery table fields values) = do
    CF.runInsertQuery table (map T.pack fields) (map T.pack values)


showUsage :: IO ()
showUsage = do 
    putStrLn "Usage: sql-csv-exe '<Command>' '<Filepath>' WHERE '<Condition>'"