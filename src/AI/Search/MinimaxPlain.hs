module AI.Search.MinimaxPlain
  ( minimaxPlain,
  )
where

import AI.Evaluation (evaluate)
import AI.Search.Common (getAllLegalMoves, pickBest)
import Data.Maybe (mapMaybe)
import GameState (GameState, applyMove, gsActiveColor)
import Move (Move)
import Piece (Color (White))

-- | Basic minimax algorithm without optimizations
minimaxPlain :: Int -> GameState -> Maybe Move
minimaxPlain depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  pure (minimaxPlainScore (depth - 1) nextState, move)
              )
              legalMoves
       in snd <$> pickBest maximizing scoredMoves

-- | Recursive scoring function for plain minimax
minimaxPlainScore :: Int -> GameState -> Int
minimaxPlainScore depth gameState
  | depth <= 0 = evaluate gameState
  | null legalMoves = evaluate gameState
  | null childScores = evaluate gameState
  | gsActiveColor gameState == White = maximum childScores
  | otherwise = minimum childScores
  where
    legalMoves = getAllLegalMoves gameState
    childScores =
      mapMaybe
        (fmap (minimaxPlainScore (depth - 1)) . applyMove gameState)
        legalMoves
