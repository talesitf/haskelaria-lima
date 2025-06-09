module Main (main) where -- Adicionada lista de exportação

import PortfolioGenerator (dowJones30, loadMultipleStocks)
import PortfolioAnalysis (printPortfolioSummary)
import DataTypes (PortfolioMetrics(..)) -- Removido StockData, Ticker não usados diretamente aqui
import Simulation (runPortfolioSimulation)
import ResultsLogger (saveResults) -- Importar o novo módulo
import Control.Monad (when)
import System.Exit (exitFailure)
import qualified Data.Map as Map
import Text.Printf (printf)
import System.Clock -- Para medição de tempo
import Data.Time.Clock (getCurrentTime) -- Para timestamp

main :: IO ()
main = do
    putStrLn "Iniciando simulação de portfólio..."
    startTime <- getTime Monotonic -- Marcar o tempo de início

    -- Carregar Dados e Configurar Parâmetros
    let allTickers = dowJones30
    let numAvailableTickers = length allTickers
    
    putStrLn $ "Carregando dados para: " ++ show allTickers
    stockData <- loadMultipleStocks allTickers
    
    when (Map.size stockData < length allTickers) $ do
        putStrLn "\nERRO: Falha ao carregar dados de todas as ações. Abortando."
        putStrLn $ "Dados carregados para " ++ show (Map.size stockData) ++ " de " ++ show (length allTickers) ++ " tickers."
        putStrLn $ "Tickers faltando: " ++ show (filter (not . (`Map.member` stockData)) allTickers)
        exitFailure
    putStrLn "Dados carregados com sucesso.\n"
    
    let numToChoose = 25      -- Quantos tickers para escolher para cada portfólio (reduzido para teste rápido)
    let numSimulations = 100 -- Quantas simulações de peso por combinação (reduzido para teste rápido)
    let batchSize = 5000      -- Quantas combinações de tickers por lote (reduzido para teste rápido)

    printf "Configurações da Simulação:\n"
    printf "  - Tickers disponíveis: %d\n" numAvailableTickers
    printf "  - Tickers para escolher por portfólio: %d\n" numToChoose
    printf "  - Simulações de peso por combinação: %d\n" numSimulations
    printf "  - Combinações por lote: %d\n\n" batchSize

    -- Executar a simulação
    (overallBestPortfolio, totalCombinations) <- runPortfolioSimulation 
                                allTickers 
                                stockData 
                                numToChoose 
                                numSimulations 
                                batchSize
    
    endTime <- getTime Monotonic -- Marcar o tempo de fim
    let timeSpec = diffTimeSpec endTime startTime
    let durationSeconds = fromIntegral (sec timeSpec) + fromIntegral (nsec timeSpec) / 1e9

    printf "\nSimulação concluída em %.2f segundos.\n" durationSeconds

    -- Apresentar Resultado Final
    putStrLn "\n--- MELHOR CARTEIRA FINAL ENCONTRADA ---"
    printPortfolioSummary overallBestPortfolio

    -- Salvar resultados
    currentTime <- getCurrentTime
    saveResults "results.csv" 
                currentTime 
                durationSeconds 
                overallBestPortfolio 
                numAvailableTickers
                numToChoose 
                numSimulations 
                batchSize 
                totalCombinations