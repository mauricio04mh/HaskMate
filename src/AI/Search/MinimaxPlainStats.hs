module AI.Search.MinimaxPlainStats
  ( minimaxPlainWithStats,
  )
where

import AI.Evaluation (evaluate)
import AI.Search.Common (getAllLegalMoves, pickBest)
import AI.SearchStats (SearchStats (..), emptySearchStats, withElapsedPicos)
import Control.Monad.State.Strict (State, modify', runState)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsResult)
import Move (Move)
import Piece (Color (White))
import System.CPUTime (getCPUTime)

-- | Plain minimax with statistics tracking
minimaxPlainWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
minimaxPlainWithStats depth gameState
  | depth <= 0 = pure (Nothing, emptySearchStats)
  | otherwise = do
      start <- getCPUTime
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          action =
            mapM
              ( \move ->
                  case applyMove gameState move of
                    Nothing ->
                      error "minimaxPlainWithStats: applyMove failed for a move reported as legal"
                    Just nextState -> do
                      score <- minimaxPlainScoreWithStats (depth - 1) 1 nextState
                      pure (score, move)
              )
              legalMoves
          (scoredMoves, stats) = runState action emptySearchStats
          bestMove = snd <$> pickBest maximizing scoredMoves
      end <- getCPUTime
      let diffPicos = end - start
      pure (bestMove, withElapsedPicos diffPicos stats)

-- | Recursive scoring with statistics
minimaxPlainScoreWithStats :: Int -> Int -> GameState -> State SearchStats Int
minimaxPlainScoreWithStats depth ply gameState = do
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
      let legalMoves = getAllLegalMoves gameState
      modify' (\stats -> stats {generatedMoves = generatedMoves stats + length legalMoves})
      if null legalMoves
        then leafEval
        else do
          childScores <-
            mapM
              ( \move ->
                  case applyMove gameState move of
                    Nothing ->
                      error "minimaxPlainScoreWithStats: applyMove failed for a move reported as legal"
                    Just nextState -> minimaxPlainScoreWithStats (depth - 1) (ply + 1) nextState
              )
              legalMoves
          if null childScores
            then leafEval
            else
              if gsActiveColor gameState == White
                then pure (maximum childScores)
                else pure (minimum childScores)
  where
    leafEval = do
      modify' (\stats -> stats {leafEvals = leafEvals stats + 1})
      pure (evaluate gameState)
