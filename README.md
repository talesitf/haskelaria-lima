# Haskelaria Lima - Análise e Otimização de Portfólio de Ações

## Descrição do Projeto

`haskelaria-lima` é uma aplicação em Haskell projetada para realizar análises de portfólios de investimento. O objetivo principal é encontrar uma carteira de ações otimizada com base em dados históricos, utilizando simulações de Monte Carlo para avaliar diferentes combinações de ativos e seus respectivos pesos. A métrica primária para otimização é o Índice de Sharpe, que mede o retorno ajustado ao risco.

O projeto utiliza dados históricos de preços de ações (inicialmente configurado para componentes do índice Dow Jones 30), calcula os retornos diários, e então simula milhares de portfólios com diferentes alocações de peso para um subconjunto escolhido de ações. O resultado final é a carteira que apresentou o maior Índice de Sharpe durante as simulações.

Funcionalidades principais:
*   Carregamento de dados históricos de preços de ações a partir de arquivos CSV.
*   Cálculo de retornos diários para cada ação.
*   Geração de combinações de tickers para formar portfólios.
*   Geração de pesos aleatórios para os ativos dentro de cada portfólio simulado.
*   Cálculo de métricas de portfólio: Retorno Anualizado, Volatilidade Anualizada e Índice de Sharpe.
*   Paralelização para acelerar o processo de simulação.
*   Processamento em lotes para gerenciar grandes números de combinações.

## Requisitos e Dependências

Para compilar e executar este projeto, você precisará de:

*   **Stack**: Uma ferramenta de desenvolvimento multiplataforma para projetos Haskell. (Recomendado)
    *   O Stack gerenciará a instalação do compilador GHC correto e todas as dependências do projeto listadas no arquivo `package.yaml` (e `haskelaria-lima.cabal`).
*   **GHC (Glasgow Haskell Compiler)**: A versão será gerenciada pelo Stack, conforme especificado no arquivo `stack.yaml`.
*   **Arquivos de Dados CSV**: O projeto espera encontrar arquivos CSV com dados históricos de preços na pasta `dados_dowjones/`. Cada arquivo deve ser nomeado com o ticker da ação (ex: `AAPL.csv`) e conter pelo menos uma coluna de preços de fechamento (o `CsvReader` está configurado para ler a 5ª coluna, índice 4, como preço de fechamento ajustado).

Principais dependências Haskell (gerenciadas pelo Stack):
*   `cassava` (para leitura de CSV)
*   `vector` (usado por `cassava`)
*   `bytestring` (usado por `cassava`)
*   `containers` (para `Data.Map`)
*   `random` (para geração de pesos aleatórios)
*   `parallel` (para `parMap` e estratégias de paralelismo)
*   `deepseq` (para avaliação profunda em contextos paralelos)
*   `async` (para paralelismo concorrente, se utilizado)

## Como Instalar

1.  **Clone o repositório (se ainda não o fez):**
    ```bash
    git clone <url-do-seu-repositorio>
    cd haskelaria-lima
    ```

2.  **Construa o projeto usando Stack:**
    Este comando fará o download do GHC necessário (se ainda não estiver presente), instalará todas as dependências e compilará o projeto.
    ```bash
    stack build
    ```
    Isso pode levar algum tempo na primeira vez, pois o Stack estará configurando o ambiente e baixando as dependências.

## Como Executar

Após a compilação bem-sucedida, você pode executar a aplicação usando o Stack:

```bash
stack exec haskelaria-lima-exe
```

A aplicação iniciará, carregará os dados das ações, realizará as simulações e, ao final, imprimirá um resumo da melhor carteira encontrada.

**Configurações da Simulação:**
Os parâmetros da simulação, como o número de tickers a serem escolhidos para cada portfólio, o número de simulações de peso por combinação e o tamanho dos lotes, são definidos diretamente no código-fonte, no arquivo `app/Main.hs`:

```haskell
-- filepath: app/Main.hs
// ...
    let numToChoose = 25      -- Quantos tickers para escolher para cada portfólio
    let numSimulations = 1000 -- Quantas simulações de peso por combinação de tickers
    let batchSize = 5000      -- Quantas combinações de tickers por lote
// ...
```
Para alterar esses parâmetros, modifique os valores no arquivo `app/Main.hs` e recompile o projeto com `stack build`.

## Resultados Esperados

Ao executar a aplicação, você verá mensagens no console indicando o progresso:
1.  Carregamento dos dados das ações.
2.  Informações sobre as configurações da simulação.
3.  Progresso do processamento dos lotes de combinações, incluindo o melhor Índice de Sharpe encontrado em cada lote e o melhor global até o momento.
4.  Tempo total da simulação.
5.  Ao final, um resumo detalhado da "MELHOR CARTEIRA FINAL ENCONTRADA" no console, incluindo:
    *   Retorno Anualizado (%)
    *   Volatilidade Anualizada (%)
    *   Índice de Sharpe
    *   Composição da Carteira (Tickers e seus respectivos pesos percentuais)
6.  Uma mensagem indicando que os resultados foram salvos em `results.csv`.

Exemplo de saída final no console:
```
Simulação concluída em XXX.XX segundos.

--- MELHOR CARTEIRA FINAL ENCONTRADA ---
============================================================
                    MELHOR CARTEIRA
============================================================
Retorno Anualizado:     XX.XX%
Volatilidade Anualizada: YY.YY%
Sharpe Ratio:           Z.ZZZZ

COMPOSIÇÃO DA CARTEIRA:
------------------------------------------------------------
Ticker   |  Peso (%)
------------------------------------------------------------
AAPL     |   10.00%
MSFT     |   15.00%
...      |    ...%
------------------------------------------------------------
TOTAL    |  100.00%
============================================================
Resultados da simulação salvos em: results.csv
```
Os valores exatos de XX.XX, YY.YY e Z.ZZZZ, bem como a composição da carteira, variarão a cada execução devido à natureza aleatória da geração de pesos.

## Salvamento dos Resultados

Além da saída no console, os resultados detalhados de cada execução da simulação são anexados ao arquivo `results.csv`, localizado na raiz do projeto. Se o arquivo não existir ou estiver vazio, um cabeçalho será adicionado.

O arquivo `results.csv` terá as seguintes colunas:

*   `Timestamp`: Data e hora em que a simulação foi concluída (formato: YYYY-MM-DD HH:MM:SS).
*   `DurationSeconds`: Tempo total da simulação em segundos.
*   `SharpeRatio`: O Índice de Sharpe da melhor carteira encontrada.
*   `AnnualReturn`: O Retorno Anualizado da melhor carteira.
*   `AnnualVolatility`: A Volatilidade Anualizada da melhor carteira.
*   `NumAvailableTickers`: Número total de tickers disponíveis para a simulação (ex: 30 para Dow Jones 30).
*   `NumToChoose`: Número de tickers selecionados para compor cada portfólio simulado.
*   `NumSimulationsPerCombo`: Número de simulações de pesos realizadas para cada combinação de tickers.
*   `BatchSize`: Número de combinações de tickers processadas por lote.
*   `TotalCombinationsRun`: Número total de combinações de tickers que foram consideradas/geradas pela simulação.
*   `Composition`: A composição da melhor carteira encontrada, formatada como uma string (ex: `"TICKER1:PESO1;TICKER2:PESO2;..."`).

Este arquivo permite um acompanhamento histórico das simulações e a análise de como diferentes parâmetros podem influenciar os resultados.

## Comparações

O objetivo principal do projeto é identificar, dentre um vasto espaço de possibilidades, a combinação de ativos e pesos que maximiza o Índice de Sharpe. Não há uma "resposta correta" única, pois os resultados dependem dos dados históricos utilizados e da aleatoriedade das simulações.

*   **Comparação entre Execuções:** Devido à geração aleatória de pesos, diferentes execuções podem produzir resultados ligeiramente diferentes. Para resultados mais estáveis, um número maior de `numSimulations` pode ser considerado, ao custo de maior tempo de processamento.
*   **Comparação com Benchmarks:** Os resultados podem ser conceitualmente comparados com o desempenho de índices de mercado (como o próprio Dow Jones 30 ou o S&P 500) no mesmo período dos dados históricos, embora o projeto não faça essa comparação diretamente. O foco é a otimização relativa dentro do universo de ativos e simulações consideradas.
*   **Impacto dos Parâmetros:**
    *   `numToChoose`: Alterar o número de ativos no portfólio pode levar a diferentes níveis de diversificação e, consequentemente, diferentes perfis de risco/retorno.
    *   `numSimulations`: Um número maior de simulações por combinação de tickers aumenta a chance de encontrar uma alocação de pesos próxima do ótimo para aquela combinação específica.

Este projeto serve como uma ferramenta para explorar estratégias de alocação de ativos baseadas em dados históricos e na teoria moderna de portfólios.
