module AI.Heuristics.Material
  ( materialScore,
    pieceValue,
  )
where

import Board.Core (Board)
import GameState.MoveGen (piecePositions)
import Piece (Color (Black, White), Piece, PieceType (..), getPieceType)

materialScore :: Board -> Int
materialScore board =
  totalValue White board - totalValue Black board

totalValue :: Color -> Board -> Int
totalValue color board =
  sum [pieceValue piece | (_, piece) <- piecePositions board color]

pieceValue :: Piece -> Int
pieceValue piece =
  case getPieceType piece of
    Pawn -> 100
    Knight -> 320
    Bishop -> 330
    Rook -> 500
    Queen -> 900
    King -> 20000
