module Simulation
    ( runPortfolioSimulation
    ) where

import DataTypes
import PortfolioAnalysis
import PortfolioGenerator
import Data.List (foldl')
import Control.Monad (replicateM, when, foldM)
import Text.Printf (printf)
import Control.Parallel.Strategies (parMap, rdeepseq)

-- Função auxiliar para dividir uma lista em lotes.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Função auxiliar para gerar combinações.
combinationsOf :: Int -> [a] -> [[a]]
combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

-- Unidade de trabalho: processa UMA combinação, executando suas simulações em PARALELO.
findBestMetricsForCombination :: (StockData, Int) -> [Ticker] -> IO PortfolioMetrics
findBestMetricsForCombination (stockData, numSimulations) tickers = do
    weightsList <- replicateM numSimulations (generateRandomValidWeights (length tickers))
    let analyzeOne weights = calculatePortfolioMetrics tickers weights stockData
    
    let metricsList = parMap rdeepseq analyzeOne weightsList
    
    -- Usar 'case' para tratar explicitamente a lista vazia e não vazia
    case metricsList of
        [] -> return $ PortfolioMetrics (-1/0) (1/0) (-1/0) [] -- Pior caso se a lista de métricas estiver vazia
        (firstMetric:restMetrics) -> 
            return $ foldl' (\m1 m2 -> if portfolioSharpeRatio m1 >= portfolioSharpeRatio m2 then m1 else m2) firstMetric restMetrics

-- Função recursiva que processa os LOTES sequencialmente.
runSimulationsInBatches :: Int -> Int -> PortfolioMetrics -> [[[Ticker]]] -> (StockData, Int) -> IO PortfolioMetrics
runSimulationsInBatches _ _ bestSoFar [] _ = do
    putStrLn "\nTodos os lotes foram processados."
    return bestSoFar
runSimulationsInBatches batchNum totalBatches bestSoFar (currentBatch:remainingBatches) workerPayload = do
    printf "Processando lote %d de %d (%d combinações)...\n" batchNum totalBatches (length currentBatch)

    -- Processa cada combinação no lote atual.
    -- A paralelização ocorre DENTRO de findBestMetricsForCombination para as simulações de peso.
    -- Aqui, as combinações dentro de um lote são processadas sequencialmente para simplificar
    -- o gerenciamento do "melhor resultado até agora" e a impressão.
    -- Se desejado, `mapM` poderia ser trocado por `mapConcurrently` do pacote `async`
    -- para paralelizar o processamento das combinações também, mas isso adicionaria outra
    -- camada de paralelismo e exigiria cuidado com a agregação dos resultados.

    let processCombo acc combo = do
            currentMetrics <- findBestMetricsForCombination workerPayload combo
            return $! if portfolioSharpeRatio currentMetrics > portfolioSharpeRatio acc
                      then currentMetrics
                      else acc
    
    bestOfCurrentBatch <- foldM processCombo (PortfolioMetrics (-1/0) (1/0) (-1/0) []) currentBatch

    let newBestSoFar = if portfolioSharpeRatio bestOfCurrentBatch > portfolioSharpeRatio bestSoFar
                       then bestOfCurrentBatch
                       else bestSoFar

    when (portfolioSharpeRatio bestOfCurrentBatch > (-1/0)) $ -- Evita imprimir para lotes vazios ou sem resultados válidos
      printf "Melhor Sharpe Ratio do lote %d: %.4f\n" batchNum (portfolioSharpeRatio bestOfCurrentBatch)
    printf "Melhor Sharpe Ratio global até agora (após lote %d): %.4f\n\n" batchNum (portfolioSharpeRatio newBestSoFar)

    runSimulationsInBatches (batchNum + 1) totalBatches newBestSoFar remainingBatches workerPayload

-- Função principal de simulação exportada
-- Modificada para retornar também o número total de combinações
runPortfolioSimulation :: [Ticker] -> StockData -> Int -> Int -> Int -> IO (PortfolioMetrics, Int)
runPortfolioSimulation allTickers stockData numToChoose numSimulations batchSize = do
    let tickerCombinations = combinationsOf numToChoose allTickers
    let totalTickerCombinations = length tickerCombinations -- Calcular o total de combinações
    let combinationBatches = chunksOf batchSize tickerCombinations
    let totalBatches = length combinationBatches
    
    printf "Total de combinações de tickers a serem geradas: %d\n" totalTickerCombinations
    printf "O trabalho foi dividido em %d lotes de aproximadamente %d combinações cada.\n\n" totalBatches batchSize

    let initialBest = PortfolioMetrics (-1/0) (1/0) (-1/0) []
    let workerPayload = (stockData, numSimulations)
    
    bestPortfolio <- runSimulationsInBatches 1 totalBatches initialBest combinationBatches workerPayload
    return (bestPortfolio, totalTickerCombinations) -- Retornar a métrica e o total de combinações