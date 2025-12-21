module AI.Search
  ( minimax,
    minimaxWithStats,
    getAllLegalMoves,
  )
where

import AI.Evaluation (evaluate)
import AI.SearchStats (SearchStats (..), emptySearchStats, withElapsedPicos)
import Control.Monad.State.Strict (State, modify', runState)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult, isLegalMove)
import GameState.MoveGen (generateMovesForPiece, piecePositions)
import Move (Move)
import Piece (Color (White))
import System.CPUTime (getCPUTime)

minimax :: Int -> GameState -> Maybe Move
minimax depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  pure (minimaxScore (depth - 1) nextState, move)
              )
              legalMoves
       in snd <$> pickBest maximizing scoredMoves

minimaxWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
minimaxWithStats depth gameState
  | depth <= 0 = pure (Nothing, emptySearchStats)
  | otherwise = do
      start <- getCPUTime
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          action =
            fmap catMaybes $
              mapM
                ( \move ->
                    case applyMove gameState move of
                      Nothing ->
                        error "minimaxWithStats: applyMove failed for a move reported as legal"
                      Just nextState -> do
                        score <- minimaxScoreWithStats (depth - 1) 1 nextState
                        pure (Just (score, move))
                )
                legalMoves
          (scoredMoves, stats) = runState action emptySearchStats
          bestMove = snd <$> pickBest maximizing scoredMoves
      end <- getCPUTime
      let diffPicos = end - start
      pure (bestMove, withElapsedPicos diffPicos stats)

minimaxScore :: Int -> GameState -> Int
minimaxScore depth gameState
  | depth <= 0 = evaluate gameState
  | gsResult gameState /= Ongoing = evaluate gameState
  | null legalMoves = evaluate gameState
  | null childScores = evaluate gameState
  | gsActiveColor gameState == White = maximum childScores
  | otherwise = minimum childScores
  where
    legalMoves = getAllLegalMoves gameState
    childScores =
      mapMaybe
        (fmap (minimaxScore (depth - 1)) . applyMove gameState)
        legalMoves

minimaxScoreWithStats :: Int -> Int -> GameState -> State SearchStats Int
minimaxScoreWithStats depth ply gameState = do
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
                      error "minimaxWithStats: applyMove failed for a move reported as legal"
                    Just nextState -> minimaxScoreWithStats (depth - 1) (ply + 1) nextState
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

pickBest :: Bool -> [(Int, Move)] -> Maybe (Int, Move)
pickBest _ [] = Nothing
pickBest True xs = Just (maximumBy (comparing fst) xs)
pickBest False xs = Just (minimumBy (comparing fst) xs)

getAllLegalMoves :: GameState -> [Move]
getAllLegalMoves gameState =
  [ move
    | (from, piece) <- piecePositions (gsBoard gameState) (gsActiveColor gameState),
      move <- generateMovesForPiece gameState from piece,
      isLegalMove gameState move
  ]
