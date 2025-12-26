module AI.Search.Negamax
  ( negamax,
    negamaxAB,
    bestMoveAB,
  )
where

import AI.Search.Common (getAllLegalMoves, inf, orderMoves, pickBestByScore, staticScore)
import Data.Maybe (mapMaybe)
import GameState (GameState, Result (..), applyMove, gsResult)
import Move (Move)

-- | Main negamax entry point (uses bestMoveAB)
negamax :: Int -> GameState -> Maybe Move
negamax = bestMoveAB

-- | Core negamax algorithm with alpha-beta pruning
negamaxAB :: Int -> Int -> Int -> GameState -> Int
negamaxAB depth alpha beta gameState
  | depth <= 0 = staticScore gameState
  | gsResult gameState /= Ongoing = staticScore gameState
  | null legalMoves = staticScore gameState
  | otherwise = go orderedMoves alpha False
  where
    legalMoves = getAllLegalMoves gameState
    orderedMoves = orderMoves gameState legalMoves
    go [] best hadChild =
      if hadChild
        then best
        else staticScore gameState
    go (move : rest) best hadChild =
      case applyMove gameState move of
        Nothing -> go rest best hadChild
        Just nextState ->
          let score = negate (negamaxAB (depth - 1) (-beta) (-best) nextState)
              best' = max best score
           in if best' >= beta
                then best'
                else go rest best' True

-- | Root-level search function
bestMoveAB :: Int -> GameState -> Maybe Move
bestMoveAB depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  let score = negate (negamaxAB (depth - 1) (-inf) inf nextState)
                  pure (score, move)
              )
              legalMoves
       in snd <$> pickBestByScore scoredMoves
