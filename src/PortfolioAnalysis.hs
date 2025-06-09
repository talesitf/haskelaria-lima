module PortfolioAnalysis 
    ( calculatePortfolioMetrics
    , printPortfolioSummary
    , calculateDailyReturns
    ) where

import DataTypes
import qualified Data.Map as Map
import Text.Printf (printf)

-- Calcula retornos diários de uma série de preços (versão segura)
calculateDailyReturns :: [Price] -> [Double]
calculateDailyReturns [] = []
calculateDailyReturns [_] = []
calculateDailyReturns prices = zipWith (\p1 p2 -> p2 / p1 - 1) prices (drop 1 prices)

-- Calcula média
mean :: [Double] -> Double
mean [] = 0
mean xs = sum xs / fromIntegral (length xs)

-- Calcula desvio padrão
stdDev :: [Double] -> Double
stdDev [] = 0
stdDev xs = sqrt $ mean $ map (\x -> (x - m) ** 2) xs
  where m = mean xs

-- Calcula métricas do portfólio
calculatePortfolioMetrics :: [Ticker] -> [Weight] -> StockData -> PortfolioMetrics
calculatePortfolioMetrics tickers weights stockData = 
    let
        -- Obtém os dados de preços para cada ticker
        stockPrices = map (\ticker -> Map.findWithDefault [] ticker stockData) tickers
        
        -- Calcula retornos para cada ação
        stockReturns = map calculateDailyReturns stockPrices
        
        -- Calcula retornos do portfólio (soma ponderada)
        portfolioReturns = calculatePortfolioReturns weights stockReturns
        
        -- Métricas anualizadas (252 dias úteis)
        annualReturn = mean portfolioReturns * 252
        annualVolatility = stdDev portfolioReturns * sqrt 252
        sharpeRatio = if annualVolatility > 0 then annualReturn / annualVolatility else 0
        
        -- Composição da carteira
        portfolioComposition = zip tickers weights
        
    in PortfolioMetrics annualReturn annualVolatility sharpeRatio portfolioComposition

-- Calcula retornos do portfólio como média ponderada
calculatePortfolioReturns :: [Weight] -> [[Double]] -> [Double]
calculatePortfolioReturns weights stockReturns = 
    let
        -- Garante que todas as séries tenham o mesmo comprimento
        -- Adicionado tratamento para stockReturns vazio ou com sublistas vazias para evitar erro em 'minimum'
        minLength = if null stockReturns || any null stockReturns 
                    then 0 
                    else minimum $ map length stockReturns
        
        truncatedReturns = map (take minLength) stockReturns
        
        -- Calcula retorno ponderado para cada dia
        -- Se minLength for 0, dailyReturns será [], o que é correto.
        dailyReturns = if minLength == 0 || null weights
                       then [] 
                       else map (sum . zipWith (*) weights) (transpose truncatedReturns)
    in dailyReturns
  where
    -- Transpõe matriz: de [[Double]] para [[Double]]
    -- Recomenda-se usar a versão com recursão em cauda da resposta anterior.
    transpose :: [[Double]] -> [[Double]]
    transpose [] = [] 
    transpose ([] : _) = [] -- Se a primeira linha está vazia, todas devem estar (devido ao take minLength)
                           -- ou quando todas as colunas foram processadas na recursão em cauda.
    transpose matrix = go matrix []
      where
        go :: [[Double]] -> [[Double]] -> [[Double]] -- Função auxiliar com acumulador
        go currentMatrix acc
            -- Se todas as listas internas estiverem vazias, terminamos.
            | all null currentMatrix = reverse acc
            | otherwise =
                let -- Extrai a primeira coluna e as matrizes restantes de forma segura
                    processRow :: [Double] -> (Maybe Double, [Double])
                    processRow [] = (Nothing, [])
                    processRow (x:xs) = (Just x, xs)

                    (firstElements, remainingLists) = unzip (map processRow currentMatrix)
                    
                    newColumn = [x | Just x <- firstElements]

                in if null newColumn && any (not . null) remainingLists
                   then reverse acc 
                   else go remainingLists (newColumn : acc)

-- Imprime resumo do portfólio
printPortfolioSummary :: PortfolioMetrics -> IO ()
printPortfolioSummary metrics = do
    putStrLn $ replicate 60 '='
    putStrLn "                    MELHOR CARTEIRA"
    putStrLn $ replicate 60 '='
    putStrLn $ printf "Retorno Anualizado:     %.2f%%" (portfolioReturn metrics * 100)
    putStrLn $ printf "Volatilidade Anualizada: %.2f%%" (portfolioVolatility metrics * 100)
    putStrLn $ printf "Sharpe Ratio:           %.4f" (portfolioSharpeRatio metrics)
    putStrLn ""
    putStrLn "COMPOSIÇÃO DA CARTEIRA:"
    putStrLn $ replicate 60 '-'
    putStrLn "Ticker   |  Peso (%)"
    putStrLn $ replicate 60 '-'
    
    mapM_ printStockLine (portfolioWeights metrics)
    
    putStrLn $ replicate 60 '-'
    let totalWeight = sum $ map snd (portfolioWeights metrics)
    putStrLn $ printf "TOTAL    | %7.2f%%" (totalWeight * 100)
    putStrLn $ replicate 60 '='

printStockLine :: (Ticker, Weight) -> IO ()
printStockLine (ticker, weight) = do
    putStrLn $ printf "%-8s | %7.2f%%" ticker (weight * 100)