module AI.Evaluation
  ( evaluate,
    mateScore,
  )
where

import AI.Heuristics.King (kingSafetyScore)
import AI.Heuristics.Material (materialScore)
import AI.Heuristics.Pawn (pawnStructureScore)
import AI.Heuristics.Positional (positionalScore)
import GameState (GameState, Result (..), gsBoard, gsCastlingRights, gsResult)
import Piece (Color (Black, White))

evaluate :: GameState -> Int
evaluate gameState =
  case gsResult gameState of
    Checkmate winner -> mateScoreFor winner
    DrawBy50Moves -> 0
    DrawByRepetition -> 0
    Stalemate -> 0
    DrawByAgreement -> 0
    Ongoing ->
      materialScore board
        + positionalScore board
        + kingSafetyScore board (gsCastlingRights gameState)
        + pawnStructureScore board
  where
    board = gsBoard gameState
    mateScoreFor White = mateScore
    mateScoreFor Black = negate mateScore

mateScore :: Int
mateScore = 100000
