{-# LANGUAGE OverloadedStrings #-}

module CsvReader 
  ( Row(..)
  , readCsvFile
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Csv                    as Csv
import qualified Data.Vector                 as V

-------------------------------------------------------------------------------
-- Tipo da linha --------------------------------------------------------------
-------------------------------------------------------------------------------

data Row = Row
  { date   :: !String
  , close  :: !Double
  , high   :: !Double
  , low    :: !Double
  , open   :: !Double
  , volume :: !Int
  } deriving (Show)

instance Csv.FromNamedRecord Row where
  parseNamedRecord m =
    Row <$> m Csv..: "Date"
        <*> m Csv..: "Close"
        <*> m Csv..: "High"
        <*> m Csv..: "Low"
        <*> m Csv..: "Open"
        <*> m Csv..: "Volume"

-------------------------------------------------------------------------------
-- Função auxiliar para sanitizar o CSV --------------------------------------
-------------------------------------------------------------------------------

cleanCsv :: BL8.ByteString -> BL8.ByteString
cleanCsv raw =
  let ls = BL8.lines raw
      header = "Date,Close,High,Low,Open,Volume"
      body   = drop 3 ls   -- pula Price / Ticker / Date linhas
  in BL8.unlines (header : body)

-------------------------------------------------------------------------------
-- Função principal para ler o CSV -------------------------------------------
-------------------------------------------------------------------------------

readCsvFile :: FilePath -> IO (Either String (V.Vector Row))
readCsvFile path = do
  putStrLn $ "Reading " ++ path ++ " (format with Price/Ticker rows)..."
  raw <- BL8.readFile path
  let csvData = cleanCsv raw
      decoded :: Either String (Csv.Header, V.Vector Row)
      decoded = Csv.decodeByName csvData
  return $ case decoded of
    Left err -> Left err
    Right (_, rows) -> Right rows