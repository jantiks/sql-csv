{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SQLParser (SQLQuery(..), Condition(..), parseSQL)
import qualified CSVFilter as CF
import Text.Parsec (parse)
import System.Environment (getArgs)
import Data.List (intercalate)
import qualified Data.Text as T
import Debug.Trace (trace)

-- main :: IO ()
-- main = do
--   -- Test INSERT
--   args <- getArgs
--   let insertSQL = "INSERT INTO test.csv (id, name) VALUES (1, 'Alice')"
--   case parse parseSQL "" insertSQL of
--     Left err  -> print err
--     Right sql -> print sql

--   -- Test UPDATE
--   let updateSQL = "UPDATE /sd/sdf/test.csv SET name='Bob' WHERE id=1"
--   case parse parseSQL "" updateSQL of
--     Left err  -> print err
--     Right sql -> print sql

main :: IO ()
main = do
  args <- getArgs
  let input = intercalate " " args
  case parse parseSQL "" input of
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
        Just (Condition f op v) -> CF.runDeleteQuery table (CF.Condition (T.pack f) (T.pack op) (T.pack v))

executeSQL (UpdateQuery table updates whereClause) = do
    case whereClause of
        Nothing -> putStrLn "UPDATE Query requires a WHERE clause"
        Just (Condition f op v) -> CF.runUpdateQuery table (map (\(fld, val) -> (T.pack fld, T.pack val)) updates) (CF.Condition (T.pack f) (T.pack op) (T.pack v))
        
executeSQL (InsertQuery table fields values) = do
    putStrLn $ "asd fileName: " ++ table
    putStrLn $ "fields: " ++ show fields
    putStrLn $ "values: " ++ show values

    CF.runInsertQuery table (map T.pack fields) (map T.pack values)


-- printSQL (UpdateQuery table updates whereClause) = do
--   putStrLn "UPDATE Query"
--   putStrLn $ "Table: " ++ show table
--   putStrLn $ "Updates: " ++ show updates
--   case whereClause of
--     Nothing -> putStrLn "No WHERE clause"
--     Just (Condition field op value) -> do
--       putStrLn "WHERE Clause:"
--       putStrLn $ "Field: " ++ field
--       putStrLn $ "Operator: " ++ op
--       putStrLn $ "Value: " ++ value

-- printSQL (InsertQuery table fields values) = do
--   putStrLn "INSERT Query"
--   putStrLn $ "Table: " ++ show table
--   putStrLn $ "Fields: " ++ show fields
--   putStrLn $ "Values: " ++ show values

-- printSQL (DeleteQuery table whereClause) = do
--   putStrLn "DELETE Query"
--   putStrLn $ "Table: " ++ show table
--   case whereClause of
--     Nothing -> putStrLn "No WHERE clause"
--     Just (Condition field op value) -> do
--       putStrLn "WHERE Clause:"
--       putStrLn $ "Field: " ++ field
--       putStrLn $ "Operator: " ++ op
--       putStrLn $ "Value: " ++ value


-- import CSVFilter
-- import qualified Data.Text as T
-- import System.Environment (getArgs)

-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         [fileName, fieldName, conditionStr] -> runCSVFilter fileName (T.pack fieldName) (T.pack conditionStr)
--         _ -> putStrLn "Usage: csvreader <fileName> <fieldName> <condition>"