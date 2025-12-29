module AI.Heuristics.Positional
  ( positionalScore,
    pstValue,
  )
where

import Board.Core (Board)
import GameState.MoveGen (piecePositions)
import Piece (Color (Black, White), Piece, PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

positionToPSTIndex :: Position -> (Int, Int)
positionToPSTIndex (Position (File f) (Rank r)) = (r - 1, f - 1)

mirrorRank :: Rank -> Rank
mirrorRank (Rank r) = Rank (9 - r)

mirrorPositionForBlack :: Position -> Position
mirrorPositionForBlack (Position file rank) = Position file (mirrorRank rank)

pstValue :: Piece -> Position -> Int
pstValue piece pos =
  case getPieceType piece of
    Pawn -> tableValue pawnPST effectivePos
    Knight -> tableValue knightPST effectivePos
    _ -> 0
  where
    effectivePos =
      case getPieceColor piece of
        White -> pos
        Black -> mirrorPositionForBlack pos

tableValue :: [[Int]] -> Position -> Int
tableValue table pos =
  let (row, col) = positionToPSTIndex pos
   in (table !! row) !! col

positionalScore :: Board -> Int
positionalScore board =
  whitePST - blackPST
  where
    whitePST = sum [pstValue piece pos | (pos, piece) <- piecePositions board White]
    blackPST = sum [pstValue piece pos | (pos, piece) <- piecePositions board Black]

pawnPST :: [[Int]]
pawnPST =
  [ [0, 0, 0, 0, 0, 0, 0, 0],
    [5, 10, 10, -20, -20, 10, 10, 5],
    [5, -5, -10, 0, 0, -10, -5, 5],
    [0, 0, 0, 20, 20, 0, 0, 0],
    [5, 5, 10, 25, 25, 10, 5, 5],
    [10, 10, 20, 30, 30, 20, 10, 10],
    [50, 50, 50, 50, 50, 50, 50, 50],
    [0, 0, 0, 0, 0, 0, 0, 0]
  ]

knightPST :: [[Int]]
knightPST =
  [ [-50, -40, -30, -30, -30, -30, -40, -50],
    [-40, -20, 0, 0, 0, 0, -20, -40],
    [-30, 0, 10, 15, 15, 10, 0, -30],
    [-30, 5, 15, 20, 20, 15, 5, -30],
    [-30, 0, 15, 20, 20, 15, 0, -30],
    [-30, 5, 10, 15, 15, 10, 5, -30],
    [-40, -20, 0, 5, 5, 0, -20, -40],
    [-50, -40, -30, -30, -30, -30, -40, -50]
  ]
