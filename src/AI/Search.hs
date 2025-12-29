{-# LANGUAGE PackageImports #-}

-- | Public API for AI search algorithms
-- Default algorithms use optimized negamax with alpha-beta pruning
module AI.Search
  ( -- * Main search functions (use optimized negamax)
    minimax,
    minimaxWithStats,

    -- * Explicit algorithm variants
    minimaxPlain,
    minimaxPlainWithStats,
    negamax,
    negamaxWithStats,
    -- Time-controlled search with Iterative Deepening
    searchBestMoveTimed,
    SearchLimits (..),
    SearchResult (..),
    StopReason (..),
    defaultSearchLimits,

    -- * Common utilities
    getAllLegalMoves,
    orderMoves,
    staticScore,
  )
where

import AI.Search.Common (getAllLegalMoves, orderMoves, staticScore)
import AI.Search.MinimaxPlain (minimaxPlain)
import AI.Search.MinimaxPlainStats (minimaxPlainWithStats)
import AI.Search.Negamax (negamax)
import AI.Search.NegamaxStats (negamaxWithStats)
import AI.Search.SearchConfig
  ( SearchLimits (..),
    SearchResult (..),
    StopReason (..),
    defaultSearchLimits,
  )
import AI.Search.Timed (searchBestMoveTimed)
import AI.SearchStats (SearchStats)
import GameState (GameState)
import Move (Move)
import System.IO.Unsafe (unsafePerformIO)

minimax :: Int -> GameState -> Maybe Move
minimax depth gameState =
  let limits = defaultSearchLimits {slMaxDepth = depth, slTimeLimitMs = 5000}
   in -- Run in IO and extract, unsafe but acceptable for this use case
      -- In practice, minimax should rarely be used directly (minimaxWithStats is preferred)
      unsafePerformIO $ do
        result <- searchBestMoveTimed limits gameState
        pure $ srBestMove result

minimaxWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
minimaxWithStats depth gameState = do
  let limits = defaultSearchLimits {slMaxDepth = depth, slTimeLimitMs = 5000}
  result <- searchBestMoveTimed limits gameState
  pure (srBestMove result, srStatsTotal result)
