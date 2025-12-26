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

    -- * Common utilities
    getAllLegalMoves,
    orderMoves,
    staticScore,
  )
where

-- Re-export from modular structure
import AI.Search.Common (getAllLegalMoves, orderMoves, staticScore)
import AI.Search.MinimaxPlain (minimaxPlain)
import AI.Search.MinimaxPlainStats (minimaxPlainWithStats)
import AI.Search.Negamax (negamax)
import AI.Search.NegamaxStats (negamaxWithStats)
import AI.SearchStats (SearchStats)
import GameState (GameState)
import Move (Move)

-- | Default minimax uses optimized negamax
minimax :: Int -> GameState -> Maybe Move
minimax = negamax

-- | Default minimaxWithStats uses optimized negamax with stats
minimaxWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
minimaxWithStats = negamaxWithStats
