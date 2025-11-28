module Board where

import Piece (Piece)
import Position (Position)

type Board = [[Cell]]

data SquareColor = Light | Dark
  deriving (Eq, Show)

data CastlingRights = CastlingRights
  { whiteKingside :: Bool,
    whiteQueenside :: Bool,
    blackKingside :: Bool,
    blackQueenside :: Bool
  }
  deriving (Eq, Show)

data Cell = Cell
  { cellPosition :: Position,
    cellPiece :: Maybe Piece,
    cellColor :: SquareColor
  }
  deriving (Eq, Show)
