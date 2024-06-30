{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SQLParser (SQLQuery(..), Condition(..), parseSQL)
import qualified CSVFilter as CF
import Text.Parsec (parse)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Text as T
import Debug.Trace (trace)
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
        Nothing -> CF.runSQLQuery table fieldNames (CF.Condition "" "=" "")
        Just (Condition f op v) ->
            CF.runSQLQuery table fieldNames (CF.Condition (T.pack f) (T.pack op) (T.pack v))
    -- print records


executeSQL (DeleteQuery table whereClause) = do
    case whereClause of
        Nothing -> putStrLn "DELETE Query requires a WHERE clause"
        Just (Condition f op v) -> 
            CF.runDeleteQuery table (CF.Condition (T.pack f) (T.pack op) (T.pack v))

executeSQL (UpdateQuery table updates whereClause) = do
    case whereClause of
        Nothing -> putStrLn "UPDATE Query requires a WHERE clause"
        Just (Condition f op v) -> 
            CF.runUpdateQuery table 
            (map (\(fld, val) -> (T.pack fld, T.pack val)) updates) 
            (CF.Condition (T.pack f) (T.pack op) (T.pack v))
        
executeSQL (InsertQuery table fields values) = do
    putStrLn $ "asd fileName: " ++ table
    putStrLn $ "fields: " ++ show fields
    putStrLn $ "values: " ++ show values

    CF.runInsertQuery table (map T.pack fields) (map T.pack values)


showUsage :: IO ()
showUsage = do 
    putStrLn "Usage: sql-csv-exe '<Command>' '<Filepath>' WHERE '<Condition>'"