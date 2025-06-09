module ResultsLogger
    ( saveResults
    ) where

import DataTypes
import Text.Printf (printf)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (withFile, IOMode(AppendMode, ReadMode), hPutStrLn, hFileSize) -- Adicionado ReadMode
import System.Directory (doesFileExist)
import Data.List (intercalate)
import Control.Monad (when) -- Adicionada importação para 'when'

formatComposition :: [(Ticker, Weight)] -> String
formatComposition weights = intercalate ";" $ map (\(t, w) -> printf "%s:%.4f" t w) weights

-- Salva os resultados da simulação em um arquivo CSV
saveResults :: FilePath
            -> UTCTime
            -> Double
            -> PortfolioMetrics
            -> Int
            -> Int
            -> Int
            -> Int
            -> Int
            -> IO ()
saveResults filepath timestamp durationSecs metrics numAvailableTickers numToChoose numSimulationsPerCombo batchSize totalCombinationsRun = do
    let timestampStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
    let line = printf "%s,%.2f,%.4f,%.4f,%.4f,%d,%d,%d,%d,%d,\"%s\"\n"
            timestampStr
            durationSecs
            (portfolioSharpeRatio metrics)
            (portfolioReturn metrics)
            (portfolioVolatility metrics)
            numAvailableTickers
            numToChoose
            numSimulationsPerCombo
            batchSize
            totalCombinationsRun
            (formatComposition $ portfolioWeights metrics)

    fileExists <- doesFileExist filepath
    needsHeader <- if fileExists
                   then withFile filepath ReadMode (\handle -> do -- ReadMode agora está no escopo
                            size <- hFileSize handle
                            return $ size == 0)
                   else return True

    withFile filepath AppendMode $ \h -> do
        when needsHeader $ -- 'when' agora está no escopo
            hPutStrLn h "Timestamp,DurationSeconds,SharpeRatio,AnnualReturn,AnnualVolatility,NumAvailableTickers,NumToChoose,NumSimulationsPerCombo,BatchSize,TotalCombinationsRun,Composition"
        hPutStrLn h line
    putStrLn $ "Resultados da simulação salvos em: " ++ filepath