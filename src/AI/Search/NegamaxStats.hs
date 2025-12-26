module AI.Search.NegamaxStats
  ( negamaxWithStats,
  )
where

import AI.Search.Common (getAllLegalMoves, inf, orderMoves, pickBestByScore, staticScore)
import AI.SearchStats (SearchStats (..), emptySearchStats, withElapsedPicos)
import Control.Monad.State.Strict (State, modify', runState)
import Data.Maybe (catMaybes)
import GameState (GameState, Result (..), applyMove, gsResult)
import Move (Move)
import System.CPUTime (getCPUTime)

-- | Negamax with statistics tracking
negamaxWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
negamaxWithStats depth gameState
  | depth <= 0 = pure (Nothing, emptySearchStats)
  | otherwise = do
      start <- getCPUTime
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
          action =
            fmap catMaybes $
              mapM
                ( \move ->
                    case applyMove gameState move of
                      Nothing ->
                        error "negamaxWithStats: applyMove failed for a move reported as legal"
                      Just nextState -> do
                        score <- negamaxABWithStats (depth - 1) (-inf) inf 1 nextState
                        pure (Just (negate score, move))
                )
                legalMoves
          (scoredMoves, stats) = runState action emptySearchStats
          bestMove = snd <$> pickBestByScore scoredMoves
      end <- getCPUTime
      let diffPicos = end - start
      pure (bestMove, withElapsedPicos diffPicos stats)

-- | Core negamax with alpha-beta pruning and statistics
negamaxABWithStats :: Int -> Int -> Int -> Int -> GameState -> State SearchStats Int
negamaxABWithStats depth alpha beta ply gameState = do
  modify'
    ( \stats ->
        stats
          { nodesVisited = nodesVisited stats + 1,
            maxPlyReached = max (maxPlyReached stats) ply
          }
    )
  if depth <= 0 || gsResult gameState /= Ongoing
    then leafEval
    else do
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
      modify' (\stats -> stats {generatedMoves = generatedMoves stats + length legalMoves})
      if null legalMoves
        then leafEval
        else go legalMoves alpha False
  where
    leafEval = do
      modify' (\stats -> stats {leafEvals = leafEvals stats + 1})
      pure (staticScore gameState)
    go [] best hadChild =
      if hadChild
        then pure best
        else leafEval
    go (move : rest) best hadChild =
      case applyMove gameState move of
        Nothing -> go rest best hadChild
        Just nextState -> do
          score <- fmap negate (negamaxABWithStats (depth - 1) (-beta) (-best) (ply + 1) nextState)
          let best' = max best score
          if best' >= beta
            then pure best'
            else go rest best' True
