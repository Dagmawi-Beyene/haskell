{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Options.Applicative
import System.Exit (exitFailure)
import CSVAnalyzer
import Data.Csv (Header)

-- | Command line options
data Options = Options
  { optFilePath :: FilePath
  , optCommand :: Command
  } deriving (Show)

data Command 
  = Analyze
  | Filter T.Text T.Text  -- column name and filter value
  deriving (Show)

-- | Parse command line options
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      ( long "file"
     <> short 'f'
     <> metavar "FILENAME"
     <> help "CSV file to analyze" )
  <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser
  ( command "analyze"
    (info (pure Analyze)
     (progDesc "Analyze CSV file and show statistics"))
 <> command "filter"
    (info parseFilter
     (progDesc "Filter CSV file by column value"))
  )

parseFilter :: Parser Command
parseFilter = Filter
  <$> strOption
      ( long "column"
     <> short 'c'
     <> metavar "COLUMN"
     <> help "Column name to filter by" )
  <*> strOption
      ( long "value"
     <> short 'v'
     <> metavar "VALUE"
     <> help "Value to filter for" )

-- | Main entry point
main :: IO ()
main = do
  options <- execParser opts
  result <- runCommand options
  case result of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitFailure
    Right () -> return ()
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
     <> progDesc "A simple CSV analyzer tool"
     <> header "csv-analyzer - analyze and filter CSV files" )

-- | Execute the command
runCommand :: Options -> IO (Either String ())
runCommand (Options filePath cmd) = do
  content <- BL.readFile filePath
  case parseCSVFile content of
    Left err -> return $ Left $ "Failed to parse CSV: " ++ err
    Right (headers, records) -> 
      case cmd of
        Analyze -> do
          let stats = analyzeCSV headers records
          printStats stats
          return $ Right ()
        Filter columnName filterValue -> do
          let filtered = filterCSVByColumn columnName filterValue headers records
          printFilteredResults headers filtered
          return $ Right ()

-- | Print CSV statistics
printStats :: CSVStats -> IO ()
printStats stats = do
  putStrLn "=== CSV Analysis Results ==="
  putStrLn $ "Number of rows: " ++ show (numRows stats)
  putStrLn $ "Number of columns: " ++ show (numColumns stats)
  putStrLn ""
  
  putStrLn "Column names:"
  mapM_ (putStrLn . ("  - " ++) . T.unpack) (columnNames stats)
  putStrLn ""
  
  if not (null (numericColumns stats))
  then do
    putStrLn "Numeric columns detected:"
    mapM_ (putStrLn . ("  - " ++) . T.unpack) (numericColumns stats)
    putStrLn ""
  else putStrLn "No numeric columns detected.\n"
  
  putStrLn "Most common values per column (top 3):"
  mapM_ printColumnStats (mostCommonValues stats)

-- | Print statistics for a single column
printColumnStats :: (T.Text, [(T.Text, Int)]) -> IO ()
printColumnStats (columnName, values) = do
  putStrLn $ "  " ++ T.unpack columnName ++ ":"
  mapM_ (\(val, count) -> putStrLn $ "    - " ++ T.unpack val ++ " (" ++ show count ++ ")") values
  putStrLn ""

-- | Print filtered results
printFilteredResults :: Header -> V.Vector (V.Vector T.Text) -> IO ()
printFilteredResults _headers records = do
  putStrLn $ "Found " ++ show (V.length records) ++ " matching rows:"
  putStrLn ""
  V.mapM_ printRow (V.take 10 records)  -- Show first 10 results
  when (V.length records > 10) $
    putStrLn $ "... and " ++ show (V.length records - 10) ++ " more rows."
  where
    printRow :: V.Vector T.Text -> IO ()
    printRow row = TIO.putStrLn $ T.intercalate ", " (V.toList row)
    
    when :: Bool -> IO () -> IO ()
    when True action = action
    when False _ = return () 