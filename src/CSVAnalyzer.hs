{-# LANGUAGE OverloadedStrings #-}

module CSVAnalyzer
  ( CSVStats(..)
  , analyzeCSV
  , filterCSVByColumn
  , parseCSVFile
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Csv hiding (lookup)
import Data.List (sort, group)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.ByteString as BS

-- | Statistics for a CSV file
data CSVStats = CSVStats
  { numRows :: Int
  , numColumns :: Int
  , columnNames :: [T.Text]
  , numericColumns :: [T.Text]
  , mostCommonValues :: [(T.Text, [(T.Text, Int)])] -- Column name with top 3 values
  } deriving (Show, Eq)

-- | Parse CSV file from ByteString
parseCSVFile :: BL.ByteString -> Either String (Header, V.Vector (V.Vector T.Text))
parseCSVFile content = 
  case decode HasHeader content of
    Left err -> Left err
    Right records -> 
      -- Extract headers from the first line manually
      let lines' = BL.split 10 content -- split by newline
          headerLine = if not (null lines') then head lines' else ""
          headerFields = BL.split 44 headerLine -- split by comma
          headers = V.fromList $ map (TE.encodeUtf8 . T.strip . TE.decodeUtf8 . BL.toStrict) headerFields
          textRecords = V.map (V.map (T.strip . TE.decodeUtf8)) records
      in Right (headers, textRecords)

-- | Analyze CSV data and return statistics
analyzeCSV :: Header -> V.Vector (V.Vector T.Text) -> CSVStats
analyzeCSV headers records = CSVStats
  { numRows = V.length records
  , numColumns = V.length headers
  , columnNames = map TE.decodeUtf8 (V.toList headers)
  , numericColumns = findNumericColumns headers records
  , mostCommonValues = getMostCommonValues headers records
  }

-- | Find columns that contain primarily numeric data
findNumericColumns :: Header -> V.Vector (V.Vector T.Text) -> [T.Text]
findNumericColumns headers records =
  let columnIndices = [0 .. V.length headers - 1]
      headerTexts = map TE.decodeUtf8 (V.toList headers)
  in mapMaybe (checkNumericColumn records headerTexts) columnIndices

checkNumericColumn :: V.Vector (V.Vector T.Text) -> [T.Text] -> Int -> Maybe T.Text
checkNumericColumn records headerTexts colIndex
  | colIndex >= length headerTexts = Nothing
  | otherwise =
      let columnData = V.mapMaybe (\row -> if V.length row > colIndex 
                                          then Just (row V.! colIndex) 
                                          else Nothing) records
          numericCount = V.length $ V.filter isNumeric columnData
          totalCount = V.length columnData
      in if totalCount > 0 && fromIntegral numericCount / fromIntegral totalCount > 0.8
         then Just (headerTexts !! colIndex)
         else Nothing
  where
    isNumeric :: T.Text -> Bool
    isNumeric text = case readMaybe (T.unpack text) :: Maybe Double of
      Just _ -> True
      Nothing -> False

-- | Get most common values for each column (top 3)
getMostCommonValues :: Header -> V.Vector (V.Vector T.Text) -> [(T.Text, [(T.Text, Int)])]
getMostCommonValues headers records =
  let columnIndices = [0 .. V.length headers - 1]
      headerTexts = map TE.decodeUtf8 (V.toList headers)
  in map (getColumnMostCommon records headerTexts) columnIndices

getColumnMostCommon :: V.Vector (V.Vector T.Text) -> [T.Text] -> Int -> (T.Text, [(T.Text, Int)])
getColumnMostCommon records headerTexts colIndex =
  let columnName = if colIndex < length headerTexts 
                   then headerTexts !! colIndex 
                   else "Unknown"
      columnData = V.mapMaybe (\row -> if V.length row > colIndex 
                                      then Just (row V.! colIndex) 
                                      else Nothing) records
      valueCounts = map (\xs -> (head xs, length xs)) . group . sort . V.toList $ columnData
      topValues = take 3 . reverse . sort $ map (\(val, count) -> (count, val)) valueCounts
  in (columnName, map (\(count, val) -> (val, count)) topValues)

-- | Filter CSV data by column value
filterCSVByColumn :: T.Text -> T.Text -> Header -> V.Vector (V.Vector T.Text) -> V.Vector (V.Vector T.Text)
filterCSVByColumn columnName filterValue headers records =
  case findColumnIndex columnName headers of
    Nothing -> records -- If column not found, return original data
    Just colIndex -> V.filter (matchesFilter colIndex filterValue) records
  where
    findColumnIndex :: T.Text -> Header -> Maybe Int
    findColumnIndex name hdrs = 
      let headerTexts = map TE.decodeUtf8 (V.toList hdrs)
      in Prelude.lookup name (zip headerTexts [0..])
    
    matchesFilter :: Int -> T.Text -> V.Vector T.Text -> Bool
    matchesFilter colIndex value row =
      if V.length row > colIndex
      then T.toLower (row V.! colIndex) == T.toLower value
      else False 