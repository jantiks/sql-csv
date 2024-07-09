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
import Control.Exception


type CSVRecord = HM.HashMap Text Text


filterCSV :: FilePath -> (CSVRecord -> Bool) -> IO (Vector CSVRecord)
filterCSV fileName condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (_, v) -> return $ V.filter condition v

recordToText :: [Text] -> CSVRecord -> Text
recordToText header record = 
    T.intercalate "," $ map (\field -> HM.lookupDefault "" field record) header


applyCondition :: SP.Condition -> CSVRecord -> Bool
applyCondition (SP.Condition expr) record = evalExpr expr
  where
    evalExpr :: SP.Expr -> Bool
    evalExpr (SP.Field "") = True  -- The case with no `where` clause for SELECT command
    evalExpr (SP.Field field) = error ("unexpected field in condition: " ++ field)
    evalExpr (SP.IntConst _) = error "unexpected integer constant in condition"
    evalExpr (SP.DoubleConst _) = error "unexpected integer constant in condition"
    evalExpr (SP.StrConst _) = error "unexpected string constant in condition"
    evalExpr (SP.BinOp op left right) =
        case op of
            "="  -> evalEquality left right
            "!=" -> not (evalEquality left right)
            ">"  -> evalComparison (>) (>) left right
            "<"  -> evalComparison (<) (<) left right
            ">=" -> evalComparison (>=) (>=) left right
            "<=" -> evalComparison (<=) (<=) left right
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

    evalComparison :: (T.Text -> T.Text -> Bool) -> (Double -> Double -> Bool) -> SP.Expr -> SP.Expr -> Bool
    evalComparison textCmp numCmp left right =
        case (evalField left, evalField right) of
            (Just (Right l), Just (Right r)) -> numCmp l r
            (Just (Left l), Just (Left r)) -> textCmp l r
            _ -> case (evalNumeric left, evalNumeric right) of
                    (Just l, Just r) -> numCmp l r
                    _ -> case (evalText left, evalText right) of
                            (Just l, Just r) -> textCmp l r
                            _ -> False

    evalLogical :: (Bool -> Bool -> Bool) -> SP.Expr -> SP.Expr -> Bool
    evalLogical lg left right = lg (evalExpr left) (evalExpr right)

    evalText :: SP.Expr -> Maybe T.Text
    evalText (SP.Field field) = 
        case HM.lookup (T.pack field) record of
            Nothing -> 
                error (
                "field '" ++ 
                field ++ 
                "' doesn't exist in the record, maybe you wanted to do string comparision instead?"
                )
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
            Just val -> readMaybe (T.unpack val)
    evalNumeric (SP.IntConst i) = Just (fromIntegral i)
    evalNumeric (SP.DoubleConst d) = Just d
    evalNumeric _ = Nothing

    evalField :: SP.Expr -> Maybe (Either T.Text Double)
    evalField (SP.Field field) = 
        case HM.lookup (T.pack field) record of
            Nothing -> Nothing
            Just val -> case readMaybe (T.unpack val) of
                Just d -> Just (Right d)  -- Parsed as Double
                Nothing -> Just (Left val)  -- Remain as Text
    evalField _ = Nothing
    
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
    result <- try (BL.readFile fileName) :: IO (Either SomeException BL.ByteString)
    case result of
        Left err -> putStrLn $ "Error reading file: " ++ show err
        Right csvData -> case decodeByName csvData of
            Left err -> putStrLn $ "Error decoding CSV data: " ++ err
            Right (header, v) -> do
                let newRecord = HM.fromList $ zip fields values
                let updatedRecords = V.snoc v newRecord
                let updatedData = encodeByName header (V.toList updatedRecords)
                BL8.writeFile fileName updatedData
                putStrLn ("Records inserted into " ++ fileName)

runUpdateQuery :: FilePath -> [(T.Text, SP.Expr)] -> SP.Condition -> IO ()
runUpdateQuery fileName updates condition = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> error err
        Right (header, v) -> do
            validateUpdates header updates
            let updatedRecords = V.map (updateIfMatches condition updates) v
            let updatedData = encodeByName header (V.toList updatedRecords)
            BL8.writeFile fileName updatedData
            putStrLn "Records updated"
  where
    validateUpdates :: Header -> [(T.Text, SP.Expr)] -> IO ()
    validateUpdates header updates =
        let headerFields = map T.decodeUtf8 (V.toList header)
            updateFields = concatMap extractFields updates
            missingFields = filter (`notElem` headerFields) updateFields
        in if null missingFields
           then return ()
           else error $ "Missing fields in CSV: " ++ show missingFields

    extractFields :: (T.Text, SP.Expr) -> [T.Text]
    extractFields (_, expr) = go expr
      where
        go (SP.Field field) = [T.pack field]
        go (SP.BinOp _ left right) = go left ++ go right
        go _ = []

    updateIfMatches :: SP.Condition -> [(T.Text, SP.Expr)] -> CSVRecord -> CSVRecord
    updateIfMatches cond updates record =
        if applyCondition cond record
        then foldr applyUpdate record updates
        else record
    
    applyUpdate :: (T.Text, SP.Expr) -> CSVRecord -> CSVRecord
    applyUpdate (field, expr) record =
        case evalExpr record expr of
            Just newValue -> HM.insert field newValue record
            Nothing -> record

    evalExpr :: CSVRecord -> SP.Expr -> Maybe Text
    evalExpr record (SP.Field field) = trace ("ASD CHECKING FUNCS") $ HM.lookup (T.pack field) record
    evalExpr _ (SP.StrConst s) = Just (T.pack s)
    evalExpr _ (SP.IntConst i) = Just (T.pack $ show i)
    evalExpr _ (SP.DoubleConst d) = Just (T.pack $ show d)
    evalExpr record (SP.BinOp op left right) =
        case op of
            "+" -> do
                l <- evalNumeric record left
                r <- evalNumeric record right
                return $ showAsText (l + r)
            "-" -> do
                l <- evalNumeric record left
                r <- evalNumeric record right
                return $ showAsText (l - r)
            "*" -> do
                l <- evalNumeric record left
                r <- evalNumeric record right
                return $ showAsText (l * r)
            "/" -> do
                l <- evalNumeric record left
                r <- evalNumeric record right
                if r /= 0
                    then return $ showAsText (l / r)
                    else Nothing
            _ -> Nothing
    evalExpr _ _ = Nothing

    evalNumeric :: CSVRecord -> SP.Expr -> Maybe Double
    evalNumeric record (SP.Field field) =
        case HM.lookup (T.pack field) record of
            Nothing -> Nothing
            Just val -> readMaybe (T.unpack val)
    evalNumeric _ (SP.IntConst i) = Just (fromIntegral i)
    evalNumeric _ (SP.DoubleConst d) = Just d
    evalNumeric record (SP.BinOp op left right) = 
        case (evalNumeric record left, evalNumeric record right) of
            (Just l, Just r) -> case op of
                "+" -> Just (l + r)
                "-" -> Just (l - r)
                "*" -> Just (l * r)
                "/" -> if r /= 0 then Just (l / r) else Nothing
                _   -> Nothing
            _ -> Nothing
    evalNumeric _ _ = Nothing

    showAsText :: Double -> Text
    showAsText d = if fromIntegral (round d :: Int) == d
                   then T.pack $ show (round d :: Int)
                   else T.pack $ show d