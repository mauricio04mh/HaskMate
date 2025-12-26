module AI.Search.TimedState
  ( Deadline,
    SearchState (..),
  )
where

import AI.Search.Ordering (OrderContext)
import AI.SearchStats (SearchStats)

-- | Deadline in picoseconds
type Deadline = Integer

-- | State that tracks both search stats and order context
data SearchState = SearchState
  { ssStats :: !SearchStats,
    ssContext :: !OrderContext,
    ssNodeCount :: !Int
  }
  deriving (Eq, Show)
