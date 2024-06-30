{-# LANGUAGE OverloadedStrings #-}

module CSVFilter (
    CSVRecord,
    filterCSV,
    recordToText,
    runSQLQuery,
    runDeleteQuery,
    runInsertQuery,
    runUpdateQuery,
    Condition(..)
) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)



type CSVRecord = HM.HashMap Text Text

data Condition = Condition Text Text Text
  deriving (Show)

filterCSV :: FilePath -> (CSVRecord -> Bool) -> IO (Vector CSVRecord)
filterCSV fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (_, v) -> return $ V.filter condition v

recordToText :: CSVRecord -> Text
recordToText record = T.intercalate "," $ map snd $ HM.toList record

applyCondition :: Condition -> CSVRecord -> Bool
applyCondition (Condition field op value) record =
    (T.null field && T.null op && T.null value) || (case HM.lookup field record of
        Nothing -> error $ "field '" ++ T.unpack field ++ "' doesn't exist in the record"
        Just val -> case op of
            "="  -> val == value
            ">"  -> maybe False (> readT value) (readTMaybe val)
            "<"  -> maybe False (< readT value) (readTMaybe val)
            ">=" -> maybe False (>= readT value) (readTMaybe val)
            "<=" -> maybe False (<= readT value) (readTMaybe val)
            _    -> False)
  where
    readT :: Text -> Int
    readT = read . T.unpack

    readTMaybe :: Text -> Maybe Int
    readTMaybe = readMaybe . T.unpack


runSQLQuery :: FilePath -> [Text] -> Condition -> IO ()
runSQLQuery fileName fields condition = do
    records <- filterCSV fileName (applyCondition condition)
    let selectFields rec = if null fields then rec
                           else HM.filterWithKey (\k _ -> k `elem` fields) rec
    V.mapM_ (TIO.putStrLn . recordToText . selectFields) records

-- Function to run DELETE query and write updated records to file
runDeleteQuery :: FilePath -> Condition -> IO ()
runDeleteQuery fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            let remainingRecords = V.filter (not . applyCondition condition) v
            let updatedData = encodeByName header (V.toList remainingRecords)
            BL8.writeFile fileName updatedData
            putStrLn $ "Records deleted"

runInsertQuery :: FilePath -> [Text] -> [Text] -> IO ()
runInsertQuery fileName fields values = do
    putStrLn $ "fileName: " ++ fileName
    putStrLn $ "fields: " ++ show fields
    putStrLn $ "values: " ++ show values

    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            let newRecord = HM.fromList $ zip fields values
            putStrLn $ "newRecord: " ++ show newRecord
            let updatedRecords = V.snoc v newRecord
            putStrLn $ "updatedRecords: " ++ show updatedRecords
            let updatedData = encodeByName header (V.toList updatedRecords)
            BL8.writeFile fileName updatedData
            putStrLn $ "Records inserted into " ++ fileName

runUpdateQuery :: FilePath -> [(Text, Text)] -> Condition -> IO ()
runUpdateQuery fileName updates condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            let updatedRecords = V.map updateIfMatches v
            let updatedData = encodeByName header (V.toList updatedRecords)
            BL8.writeFile fileName updatedData
            putStrLn "Records updated"
  where
    updateIfMatches record =
        if applyCondition condition record
        then foldr (uncurry HM.insert) record updates
        else record