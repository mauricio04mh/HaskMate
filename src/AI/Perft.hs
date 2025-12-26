module AI.Perft
  ( perft,
    perftDivide,
  )
where

import AI.Search (getAllLegalMoves)
import GameState (GameState, applyMove)
import Move (Move)

perft :: Int -> GameState -> Integer
perft 0 _ = 1
perft depth gameState
  | depth < 0 = 0
  | null legalMoves = 0
  | otherwise =
      sum
        [ perft (depth - 1) nextState
          | move <- legalMoves,
            Just nextState <- [checkedApplyMove gameState move]
        ]
  where
    legalMoves = getAllLegalMoves gameState

perftDivide :: Int -> GameState -> [(Move, Integer)]
perftDivide depth gameState
  | depth <= 0 = []
  | otherwise =
      [ (move, perft (depth - 1) nextState)
        | move <- legalMoves,
          Just nextState <- [checkedApplyMove gameState move]
      ]
  where
    legalMoves = getAllLegalMoves gameState

checkedApplyMove :: GameState -> Move -> Maybe GameState
checkedApplyMove gameState move =
  case applyMove gameState move of
    Nothing ->
      error "perft: applyMove failed for a move reported as legal"
    Just nextState -> Just nextState
