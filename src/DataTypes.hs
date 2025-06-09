module DataTypes 
    ( Ticker
    , Price
    , Weight
    , StockData
    , PortfolioMetrics(..)
    ) where

import qualified Data.Map as Map
import Control.DeepSeq (NFData, rnf) -- Adicionar importação

type Price = Double
type Ticker = String
type Weight = Double
type StockData = Map.Map Ticker [Price]

data PortfolioMetrics = PortfolioMetrics
    { portfolioReturn :: Double
    , portfolioVolatility :: Double
    , portfolioSharpeRatio :: Double
    , portfolioWeights :: [(Ticker, Weight)]
    } deriving (Show, Eq)

-- Instância NFData para avaliação profunda em estratégias paralelas
instance NFData PortfolioMetrics where
    rnf metric = rnf (portfolioReturn metric) `seq`
                 rnf (portfolioVolatility metric) `seq`
                 rnf (portfolioSharpeRatio metric) `seq`
                 rnf (portfolioWeights metric) -- rnf para listas e tuplas já existe

-- Para fins de ordenação, por exemplo, encontrar o máximo Sharpe Ratio
instance Ord PortfolioMetrics where
    compare m1 m2 = compare (portfolioSharpeRatio m1) (portfolioSharpeRatio m2)