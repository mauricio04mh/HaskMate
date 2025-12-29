module AI.SearchStats
  ( SearchStats (..),
    emptySearchStats,
    withElapsedPicos,
  )
where

data SearchStats = SearchStats
  { nodesVisited :: !Int,
    leafEvals :: !Int,
    generatedMoves :: !Int,
    maxPlyReached :: !Int,
    elapsedPicos :: !Integer
  }
  deriving (Eq, Show)

emptySearchStats :: SearchStats
emptySearchStats = SearchStats 0 0 0 0 0

withElapsedPicos :: Integer -> SearchStats -> SearchStats
withElapsedPicos picos stats = stats {elapsedPicos = picos}
