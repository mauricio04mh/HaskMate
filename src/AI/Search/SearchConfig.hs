module AI.Search.SearchConfig
  ( SearchLimits (..),
    SearchResult (..),
    StopReason (..),
    defaultSearchLimits,
  )
where

import AI.SearchStats (SearchStats)
import Move (Move)

data SearchLimits = SearchLimits
  { slMaxDepth :: !Int,
    slTimeLimitMs :: !Int,
    slEnableKiller :: !Bool,
    slEnableHistory :: !Bool,
    slCheckEveryN :: !Int
  }
  deriving (Eq, Show)

data StopReason
  = CompletedDepth
  | TimeUp
  deriving (Eq, Show)

data SearchResult = SearchResult
  { -- | Best move found (Nothing if no legal moves or depth 0)
    srBestMove :: !(Maybe Move),
    -- | Best score from active player's perspective
    srBestScore :: !Int,
    -- | Last complete depth reached
    srDepthReached :: !Int,
    -- | Why the search stopped
    srStopReason :: !StopReason,
    -- | Accumulated statistics from all completed iterations
    srStatsTotal :: !SearchStats
  }
  deriving (Eq, Show)

defaultSearchLimits :: SearchLimits
defaultSearchLimits =
  SearchLimits
    { slMaxDepth = 6,
      slTimeLimitMs = 200,
      slEnableKiller = True,
      slEnableHistory = True,
      slCheckEveryN = 1024
    }
