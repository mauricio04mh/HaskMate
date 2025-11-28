module Move where

import Piece (PieceType)
import Position (Position)

data Move = Move
  { fromPos :: Position,
    toPos :: Position,
    promotion :: Maybe PieceType
  }
  deriving (Eq, Show)
