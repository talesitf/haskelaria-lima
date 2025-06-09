module PortfolioGenerator 
    ( generateRandomValidWeights
    , dowJones30
    , loadMultipleStocks
    ) where

import DataTypes
import CsvReader (readCsvFile, Row(..))
import qualified Data.Map as Map
import qualified Data.Vector as V
import System.Random (newStdGen, randomRs)
import Control.Monad (foldM)

-- As 30 ações do Dow Jones (conforme definidas no projeto)
dowJones30 :: [Ticker]
dowJones30 = 
    [ "AAPL", "AXP", "BA", "CAT", "CRM"
    , "CSCO", "CVX", "DIS", "DOW", "GS"
    , "HD", "IBM", "INTC", "JNJ", "JPM"
    , "KO", "MCD", "MMM", "MRK", "MSFT"
    , "NKE", "PFE", "PG", "RTX", "TRV"
    , "UNH", "V", "VZ", "WBA", "WMT"
    ]

-- Carrega dados de múltiplas ações a partir de arquivos CSV.
loadMultipleStocks :: [Ticker] -> IO StockData
loadMultipleStocks tickers = do
    putStrLn $ "Carregando dados de " ++ show (length tickers) ++ " ações..."
    -- Usa foldM para carregar sequencialmente e acumular os resultados.
    stockMap <- foldM loadSingleStock Map.empty tickers
    let loadedCount = Map.size stockMap
    putStrLn $ "Carregadas com sucesso: " ++ show loadedCount ++ " ações"
    return stockMap
  where
    loadSingleStock acc ticker = do
        let filePath = "dados_dowjones/" ++ ticker ++ ".csv"
        result <- readCsvFile filePath
        case result of
            Left err -> do
                putStrLn $ "Erro ao carregar " ++ ticker ++ ": " ++ err
                return acc
            Right rows -> do
                -- Os preços são extraídos e ordenados do mais antigo para o mais recente.
                let prices = reverse $ map close (V.toList rows)
                putStrLn $ ticker ++ ": " ++ show (length prices) ++ " preços carregados"
                return $ Map.insert ticker prices acc

-- Gera um vetor de pesos aleatórios que satisfaz as restrições do projeto. 
-- 1. Soma dos pesos é 1.
-- 2. Nenhum peso pode exceder 0.2 (20%).
generateRandomValidWeights :: Int -> IO [Weight]
generateRandomValidWeights n = do
    gen <- newStdGen
    let randoms = randomRs (0.0, 1.0) gen :: [Double]
        -- Pega n números aleatórios, normaliza para que a soma seja 1.
        weights = normalize $ take n randoms
    -- Usa "rejection sampling": se algum peso violar a restrição w_i <= 0.2, tenta de novo.
    if any (> 0.2) weights
        then generateRandomValidWeights n -- Recursão para tentar novamente.
        else return weights              -- Retorna os pesos válidos.
  where
    normalize :: [Double] -> [Double]
    normalize [] = []
    normalize xs = let s = sum xs in map (/ s) xs