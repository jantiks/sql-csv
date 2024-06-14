module Main (main) where
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String
import qualified Data.Csv as Csv
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of
        [queryStr] -> do
            putStr "ASD"
            putStrLn queryStr
            -- csvResult <- readCSV filePath
            -- case csvResult of
            --     Left err -> putStrLn err
            --     Right v -> case parseQuery queryStr of
            --         Left err -> print err
            --         Right query -> processQuery query v
        _ -> putStrLn "Usage: runghc Main.hs '<query>' <file>"
