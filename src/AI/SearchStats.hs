module AI.SearchStats
  ( SearchStats (..),
    emptySearchStats,
    withElapsedMs,
  )
where

data SearchStats = SearchStats
  { nodesVisited :: !Int,
    leafEvals :: !Int,
    generatedMoves :: !Int,
    maxPlyReached :: !Int,
    elapsedMs :: !Integer
  }
  deriving (Eq, Show)

emptySearchStats :: SearchStats
emptySearchStats = SearchStats 0 0 0 0 0

withElapsedMs :: Integer -> SearchStats -> SearchStats
withElapsedMs ms stats = stats {elapsedMs = ms}
