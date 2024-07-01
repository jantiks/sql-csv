{-# LANGUAGE OverloadedStrings #-}

module CSVFilter (
    CSVRecord,
    filterCSV,
    recordToText,
    runSQLQuery,
    runDeleteQuery,
    runInsertQuery,
    runUpdateQuery,
) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Debug.Trace (trace)

import qualified SQLParser as SP


type CSVRecord = HM.HashMap Text Text


filterCSV :: FilePath -> (CSVRecord -> Bool) -> IO (Vector CSVRecord)
filterCSV fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (_, v) -> return $ V.filter condition v

recordToText :: CSVRecord -> Text
recordToText record = T.intercalate "," $ map snd $ HM.toList record

stripQuotes :: Text -> Text
stripQuotes = T.strip . T.dropAround (== '"')

applyCondition :: SP.Condition -> CSVRecord -> Bool
applyCondition (SP.Condition expr) record =
    evalExpr expr
  where
    evalExpr :: SP.Expr -> Bool
    evalExpr (SP.Field field) = error $ "Field '" ++ field ++ "' cannot be evaluated directly"
    evalExpr (SP.IntConst _) = error "Integer constant cannot be evaluated directly"
    evalExpr (SP.StrConst _) = error "String constant cannot be evaluated directly"
    evalExpr (SP.BinOp op left right) =
        case op of
            "="  -> evalComparison (==)
            ">"  -> evalComparison (>)
            "<"  -> evalComparison (<)
            ">=" -> evalComparison (>=)
            "<=" -> evalComparison (<=)
            "!=" -> evalComparison (/=)
            "and" -> evalLogical (&&)
            "or"  -> evalLogical (||)
            _     -> error $ "unsupported operator: " ++ op
      where
        evalComparison cmp =
            case (evalArithmetic left, evalArithmetic right) of
                (Just l, Just r) -> l `cmp` r
                _ -> False
        
        evalLogical lg =
            case (evalExpr left, evalExpr right) of
                (True, True) -> lg True True
                (True, False) -> lg True False
                (False, True) -> lg False True
                (False, False) -> lg False False
        
        evalArithmetic (SP.BinOp op l r) =
            case (evalArithmetic l, evalArithmetic r) of
                (Just lv, Just rv) -> case op of
                    "+" -> Just (lv + rv)
                    "-" -> Just (lv - rv)
                    "*" -> Just (lv * rv)
                    "/" -> if rv /= 0 then Just (lv / rv) else Nothing
                    _   -> Nothing
                _ -> Nothing
        evalArithmetic (SP.Field field) = HM.lookup (T.pack field) record >>= readTMaybe . T.unpack
        evalArithmetic (SP.IntConst i) = Just (fromIntegral i)
        evalArithmetic (SP.StrConst s) = readTMaybe s
        evalArithmetic _ = Nothing

    evalExpr (SP.UnOp op expr) =
        case op of
            "not" -> not (evalExpr expr)
            _     -> error $ "unsupported operator: " ++ op

    readTMaybe :: Read a => String -> Maybe a
    readTMaybe = readMaybe


runSQLQuery :: FilePath -> [Text] -> SP.Condition -> IO ()
runSQLQuery fileName fields condition = do
    putStrLn $ "runSQLQuery with cond: " ++ show condition
    records <- filterCSV fileName (applyCondition condition)
    let selectFields rec = if null fields then rec
                           else HM.filterWithKey (\k _ -> k `elem` fields) rec
    V.mapM_ (TIO.putStrLn . recordToText . selectFields) records

-- Function to run DELETE query and write updated records to file
runDeleteQuery :: FilePath -> SP.Condition -> IO ()
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

runUpdateQuery :: FilePath -> [(Text, Text)] -> SP.Condition -> IO ()
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