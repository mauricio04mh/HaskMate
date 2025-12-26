module GameState.Validation
  ( isKingInCheck,
    boardIsInCheck,
    isSquareUnderAttack,
    movePosition,
    fileIndex,
    rankIndex,
    knightOffsets,
    kingOffsets,
    rookDirections,
    bishopDirections,
  )
where

import Board.Core (Board, cellPiece, cellPosition)
import Board.Query (boardPieceAt)
import Data.Maybe (listToMaybe, mapMaybe)
import GameState.Types (toggleColor)
import Piece (Color (Black, White), PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

isKingInCheck :: Board -> Color -> Bool
isKingInCheck = boardIsInCheck

boardIsInCheck :: Board -> Color -> Bool
boardIsInCheck board color =
  case findKingPosition board color of
    Nothing -> False
    Just kingPos -> isSquareUnderAttack board kingPos (toggleColor color)

findKingPosition :: Board -> Color -> Maybe Position
findKingPosition board color =
  listToMaybe
    [ cellPosition cell
      | row <- board,
        cell <- row,
        Just piece <- [cellPiece cell],
        getPieceColor piece == color,
        getPieceType piece == King
    ]

isSquareUnderAttack :: Board -> Position -> Color -> Bool
isSquareUnderAttack board target attacker =
  pawnThreats board target attacker
    || knightThreats board target attacker
    || kingThreats board target attacker
    || any (rayAttack board target attacker [Rook, Queen]) rookDirections
    || any (rayAttack board target attacker [Bishop, Queen]) bishopDirections

pawnThreats :: Board -> Position -> Color -> Bool
pawnThreats board target attacker =
  any (squareAttackedBy board attacker [Pawn]) $
    mapMaybe (movePosition target) (pawnAttackOffsets attacker)

knightThreats :: Board -> Position -> Color -> Bool
knightThreats board target attacker =
  any (squareAttackedBy board attacker [Knight]) $
    mapMaybe (movePosition target) knightOffsets

kingThreats :: Board -> Position -> Color -> Bool
kingThreats board target attacker =
  any (squareAttackedBy board attacker [King]) $
    mapMaybe (movePosition target) kingOffsets

squareAttackedBy :: Board -> Color -> [PieceType] -> Position -> Bool
squareAttackedBy board attacker types pos =
  case boardPieceAt board pos of
    Just piece -> getPieceColor piece == attacker && getPieceType piece `elem` types
    Nothing -> False

rayAttack :: Board -> Position -> Color -> [PieceType] -> (Int, Int) -> Bool
rayAttack board start attacker types direction =
  case movePosition start direction of
    Nothing -> False
    Just nextPos ->
      case boardPieceAt board nextPos of
        Nothing -> rayAttack board nextPos attacker types direction
        Just piece -> getPieceColor piece == attacker && getPieceType piece `elem` types

movePosition :: Position -> (Int, Int) -> Maybe Position
movePosition (Position (File f) (Rank r)) (df, dr) = mkPosition (f + df) (r + dr)
  where
    mkPosition x y
      | x >= 1 && x <= 8 && y >= 1 && y <= 8 = Just (Position (File x) (Rank y))
      | otherwise = Nothing

pawnAttackOffsets :: Color -> [(Int, Int)]
pawnAttackOffsets White = [(-1, -1), (1, -1)]
pawnAttackOffsets Black = [(-1, 1), (1, 1)]

knightOffsets :: [(Int, Int)]
knightOffsets =
  [ (-2, -1),
    (-2, 1),
    (-1, -2),
    (-1, 2),
    (1, -2),
    (1, 2),
    (2, -1),
    (2, 1)
  ]

kingOffsets :: [(Int, Int)]
kingOffsets =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

rookDirections :: [(Int, Int)]
rookDirections = [(1, 0), (-1, 0), (0, 1), (0, -1)]

bishopDirections :: [(Int, Int)]
bishopDirections = [(1, 1), (1, -1), (-1, 1), (-1, -1)]

fileIndex :: File -> Int
fileIndex (File f) = f

rankIndex :: Rank -> Int
rankIndex (Rank r) = r
