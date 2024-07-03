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
import qualified Data.Text.Encoding as T


type CSVRecord = HM.HashMap Text Text


filterCSV :: FilePath -> (CSVRecord -> Bool) -> IO (Vector CSVRecord)
filterCSV fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (_, v) -> return $ V.filter condition v

recordToText :: [Text] -> CSVRecord -> Text
recordToText header record = T.intercalate "," $ map (\field -> HM.lookupDefault "" field record) header


applyCondition :: SP.Condition -> CSVRecord -> Bool
applyCondition (SP.Condition expr) record = evalExpr expr
  where
    evalExpr :: SP.Expr -> Bool
    evalExpr (SP.Field "") = True  -- The case with no `where` clause for SELECT command
    evalExpr (SP.Field field) = error ("unexpected field in condition: " ++ field)
    evalExpr (SP.IntConst _) = error "unexpected integer constant in condition"
    evalExpr (SP.StrConst _) = error "unexpected string constant in condition"
    evalExpr (SP.BinOp op left right) =
        case op of
            "="  -> evalEquality left right
            "!=" -> not (evalEquality left right)
            ">"  -> evalComparison (>) left right
            "<"  -> evalComparison (<) left right
            ">=" -> evalComparison (>=) left right
            "<=" -> evalComparison (<=) left right
            "and" -> evalLogical (&&) left right
            "or"  -> evalLogical (||) left right
            _     -> error $ "unsupported operator: " ++ op

    evalExpr (SP.UnOp op expr) =
        case op of
            "not" -> not (evalExpr expr)
            _     -> error $ "Unsupported operator: " ++ op

    evalEquality :: SP.Expr -> SP.Expr -> Bool
    evalEquality left right =
        case (evalNumeric left, evalNumeric right) of
            (Just l, Just r) -> l == r
            _ -> case (evalText left, evalText right) of
                    (Just l, Just r) -> l == r
                    _ -> trace "Comparison failed" False


    evalComparison :: (T.Text -> T.Text -> Bool) -> SP.Expr -> SP.Expr -> Bool
    evalComparison cmp left right =
        case (evalNumeric left, evalNumeric right) of
            (Just l, Just r) -> cmp (T.pack $ show l) (T.pack $ show r)
            _ -> case (evalText left, evalText right) of
                    (Just l, Just r) -> cmp l r
                    _ -> False

    evalLogical :: (Bool -> Bool -> Bool) -> SP.Expr -> SP.Expr -> Bool
    evalLogical lg left right = lg (evalExpr left) (evalExpr right)

    evalText :: SP.Expr -> Maybe T.Text
    evalText (SP.Field field) = 
        case HM.lookup (T.pack field) record of
            Nothing -> error $ "field '" ++ field ++ "' doesn't exist in the record"
            Just val -> Just val
    evalText (SP.StrConst s) = Just (T.pack s)
    evalText (SP.IntConst i) = Just (T.pack $ show i)
    evalText _ = Nothing

    evalNumeric :: SP.Expr -> Maybe Double
    evalNumeric (SP.BinOp op l r) =
        case (evalNumeric l, evalNumeric r) of
            (Just lv, Just rv) -> case op of
                "+" -> Just (lv + rv)
                "-" -> Just (lv - rv)
                "*" -> Just (lv * rv)
                "/" -> if rv /= 0 then Just (lv / rv) else Nothing
                _   -> Nothing
            _ -> Nothing
    evalNumeric (SP.Field field) = 
        case HM.lookup (T.pack field) record of
            Nothing -> error $ "Field '" ++ field ++ "' doesn't exist in the record"
            Just val -> readTMaybe (T.unpack val)
    evalNumeric (SP.IntConst i) = Just (fromIntegral i)
    evalNumeric _ = Nothing

    readTMaybe :: Read a => String -> Maybe a
    readTMaybe = readMaybe
    
runSQLQuery :: FilePath -> [Text] -> SP.Condition -> IO ()
runSQLQuery fileName fields condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            let headerText = V.map T.decodeUtf8 header
            let records = V.filter (applyCondition condition) v
            let selectedHeader = if null fields
                                 then V.toList headerText
                                 else filter (`elem` fields) (V.toList headerText)
            let selectFields rec = if null fields then rec
                                   else HM.filterWithKey (\k _ -> k `elem` fields) rec
            V.mapM_ (TIO.putStrLn . recordToText selectedHeader . selectFields) records


runDeleteQuery :: FilePath -> SP.Condition -> IO ()
runDeleteQuery fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            let remainingRecords = V.filter (not . applyCondition condition) v
            let updatedData = encodeByName header (V.toList remainingRecords)
            BL8.writeFile fileName updatedData
            putStrLn "Records deleted"

runInsertQuery :: FilePath -> [Text] -> [Text] -> IO ()
runInsertQuery fileName fields values = do
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
            putStrLn ("Records inserted into " ++ fileName)

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