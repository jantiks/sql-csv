{-# LANGUAGE OverloadedStrings #-}

module CSVFilter (
    CSVRecord,
    filterCSV,
    recordToText,
    runSQLQuery,
    Condition(..)
) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Control.Monad (mapM_)


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
    case HM.lookup field record of
        Nothing -> False
        Just val -> case op of
            "="  -> val == value
            ">"  -> maybe False (> (readT value)) (readTMaybe val)
            "<"  -> maybe False (< (readT value)) (readTMaybe val)
            _    -> False
  where
    readT :: Text -> Int
    readT = read . T.unpack

    readTMaybe :: Text -> Maybe Int
    readTMaybe = readMaybe . T.unpack

runSQLQuery :: FilePath -> [Text] -> Condition -> IO ()
runSQLQuery fileName fields condition = do
    putStrLn $ "File name: " ++ fileName
    putStrLn $ "Fields: " ++ show fields
    putStrLn $ "Condition: " ++ show condition

    records <- filterCSV fileName (applyCondition condition)

    let selectFields rec = if null fields then rec
                           else HM.filterWithKey (\k _ -> k `elem` fields) rec

    V.mapM_ (TIO.putStrLn . recordToText . selectFields) records
