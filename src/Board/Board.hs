module Board.Board where

import Piece (Color (Black, White), Piece (..), PieceType (..))
import Position (File (..), Position (..), Rank (..))

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

emptyBoard :: Board
emptyBoard =
  [ [ cellAt f r
      | f <- [1 .. 8]
    ]
    | r <- [1 .. 8]
  ]
  where
    cellAt f r =
      let pos = Position (File f) (Rank r)
          sqColor = if even (f + r) then Dark else Light
       in Cell pos (pieceAt f r) sqColor

    pieceAt f r
      | r == 2 = Just $ Piece White Pawn
      | r == 7 = Just $ Piece Black Pawn
      | r == 1 = backRank White f
      | r == 8 = backRank Black f
      | otherwise = Nothing

    backRank color file =
      case file of
        1 -> Just $ Piece color Rook
        2 -> Just $ Piece color Knight
        3 -> Just $ Piece color Bishop
        4 -> Just $ Piece color Queen
        5 -> Just $ Piece color King
        6 -> Just $ Piece color Bishop
        7 -> Just $ Piece color Knight
        8 -> Just $ Piece color Rook
        _ -> Nothing
