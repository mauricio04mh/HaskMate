module GameState.FEN
  ( parseFEN,
  )
where

import qualified Data.Map.Strict as Map

import Data.Char (digitToInt, isDigit)

import Board.Core (Board, CastlingRights (..), Cell (..), SquareColor (..))
import GameState.Types (GameState (..), Result (..), positionKey)
import Piece (Color (Black, White), Piece (..), PieceType (..))
import Position (File (..), Position (..), Rank (..), mkPosition)

parseFEN :: String -> Either String GameState
parseFEN fen = do
  (boardField, activeField, castlingField, enPassantField, halfmoveField, fullmoveField) <- parseFields fen
  board <- parseBoard boardField
  activeColor <- parseActiveColor activeField
  castlingRights <- parseCastling castlingField
  enPassantTarget <- parseEnPassant enPassantField
  halfmoveClock <- parseIntWithDefault 0 halfmoveField
  fullmoveNumber <- parseIntWithDefault 1 fullmoveField
  let baseState =
        GameState
          { gsBoard = board,
            gsActiveColor = activeColor,
            gsCastlingRights = castlingRights,
            gsEnPassantTarget = enPassantTarget,
            gsHalfmoveClock = halfmoveClock,
            gsFullmoveNumber = fullmoveNumber,
            gsResult = Ongoing,
            gsPositionCounts = Map.empty
          }
      key = positionKey board activeColor castlingRights enPassantTarget
  pure baseState {gsPositionCounts = Map.singleton key 1}

parseFields :: String -> Either String (String, String, String, String, String, String)
parseFields fen =
  case words fen of
    [boardField, activeField, castlingField, enPassantField, halfmoveField, fullmoveField] ->
      Right (boardField, activeField, castlingField, enPassantField, halfmoveField, fullmoveField)
    [boardField, activeField, castlingField, enPassantField] ->
      Right (boardField, activeField, castlingField, enPassantField, "0", "1")
    _ -> Left "FEN must have 4 or 6 fields"

parseBoard :: String -> Either String Board
parseBoard boardField = do
  let rankFields = splitOnSlash boardField
  if length rankFields /= 8
    then Left "FEN board must have 8 ranks"
    else do
      parsedRanks <- mapM parseRank rankFields
      let ranksFrom1 = reverse parsedRanks
      pure [buildRank r pieces | (r, pieces) <- zip [1 .. 8] ranksFrom1]

buildRank :: Int -> [Maybe Piece] -> [Cell]
buildRank rankNum pieces =
  [ cellAt fileIdx piece
    | (fileIdx, piece) <- zip [1 .. 8] pieces
  ]
  where
    cellAt fileIdx piece =
      let pos = Position (File fileIdx) (Rank rankNum)
          sqColor = if even (fileIdx + rankNum) then Dark else Light
       in Cell pos piece sqColor

parseRank :: String -> Either String [Maybe Piece]
parseRank = go 0 []
  where
    go count acc [] =
      if count == 8
        then Right acc
        else Left "FEN rank does not have 8 files"
    go count acc (c : cs)
      | isDigit c =
        let n = digitToInt c
         in if n >= 1 && n <= 8 && count + n <= 8
              then go (count + n) (acc ++ replicate n Nothing) cs
              else Left "FEN rank has invalid empty count"
      | otherwise =
        case pieceFromChar c of
          Just piece ->
            if count + 1 <= 8
              then go (count + 1) (acc ++ [Just piece]) cs
              else Left "FEN rank has too many files"
          Nothing -> Left "FEN rank has invalid piece"

pieceFromChar :: Char -> Maybe Piece
pieceFromChar c =
  case c of
    'P' -> Just (Piece White Pawn)
    'N' -> Just (Piece White Knight)
    'B' -> Just (Piece White Bishop)
    'R' -> Just (Piece White Rook)
    'Q' -> Just (Piece White Queen)
    'K' -> Just (Piece White King)
    'p' -> Just (Piece Black Pawn)
    'n' -> Just (Piece Black Knight)
    'b' -> Just (Piece Black Bishop)
    'r' -> Just (Piece Black Rook)
    'q' -> Just (Piece Black Queen)
    'k' -> Just (Piece Black King)
    _ -> Nothing

parseActiveColor :: String -> Either String Color
parseActiveColor "w" = Right White
parseActiveColor "b" = Right Black
parseActiveColor _ = Left "FEN active color must be w or b"

parseCastling :: String -> Either String CastlingRights
parseCastling "-" = Right (CastlingRights False False False False)
parseCastling rights =
  let has c = c `elem` rights
   in Right
        ( CastlingRights
            { whiteKingside = has 'K',
              whiteQueenside = has 'Q',
              blackKingside = has 'k',
              blackQueenside = has 'q'
            }
        )

parseEnPassant :: String -> Either String (Maybe Position)
parseEnPassant "-" = Right Nothing
parseEnPassant [fileChar, rankChar] = do
  fileIdx <- parseFile fileChar
  rankIdx <- parseRankChar rankChar
  case mkPosition fileIdx rankIdx of
    Just pos -> Right (Just pos)
    Nothing -> Left "FEN en passant square is out of range"
parseEnPassant _ = Left "FEN en passant square must be '-' or a coordinate"

parseFile :: Char -> Either String Int
parseFile c =
  case c of
    'a' -> Right 1
    'b' -> Right 2
    'c' -> Right 3
    'd' -> Right 4
    'e' -> Right 5
    'f' -> Right 6
    'g' -> Right 7
    'h' -> Right 8
    _ -> Left "FEN file must be a-h"

parseRankChar :: Char -> Either String Int
parseRankChar c
  | c >= '1' && c <= '8' = Right (digitToInt c)
  | otherwise = Left "FEN rank must be 1-8"

parseIntWithDefault :: Int -> String -> Either String Int
parseIntWithDefault defaultValue field =
  if all isDigit field && not (null field)
    then Right (read field)
    else Right defaultValue

splitOnSlash :: String -> [String]
splitOnSlash str =
  case break (== '/') str of
    (before, []) -> [before]
    (before, _ : rest) -> before : splitOnSlash rest
