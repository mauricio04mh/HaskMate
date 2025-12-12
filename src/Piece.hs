module Piece where

data Color = White | Black
  deriving (Eq, Show, Ord)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Show, Ord)

data Piece = Piece
  { pieceColor :: Color,
    pieceType :: PieceType
  }
  deriving (Eq, Show, Ord)

getPieceColor :: Piece -> Color
getPieceColor (Piece color _) = color

getPieceType :: Piece -> PieceType
getPieceType (Piece _ pType) = pType
