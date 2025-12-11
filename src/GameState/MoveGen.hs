module GameState.MoveGen
  ( piecePositions,
    generateMovesForPiece,
    determineEnPassantTarget,
    isValidEnPassantCapture,
    enPassantCaptureSquare,
    pawnForwardDir,
    promotionOptions,
  )
where

import Data.Maybe (maybeToList)

import Board (Board, CastlingRights, cellPiece, cellPosition)
import Board.Query (boardPieceAt)
import Move (Move (..))
import Piece (Color, Piece (..), PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

import GameState.Types (GameState (..), toggleColor)
import GameState.Validation
  ( bishopDirections,
    fileIndex,
    isSquareUnderAttack,
    kingOffsets,
    knightOffsets,
    movePosition,
    rankIndex,
    rookDirections,
  )

piecePositions :: Board -> Color -> [(Position, Piece)]
piecePositions board color =
  [ (cellPosition cell, piece)
    | row <- board,
      cell <- row,
      Just piece <- [cellPiece cell],
      getPieceColor piece == color
  ]

generateMovesForPiece :: GameState -> Position -> Piece -> [Move]
generateMovesForPiece state from piece =
  [ Move
      { fromPos = from,
        toPos = to,
        promotion = promo
      }
    | to <- pieceDestinations state from piece,
      promo <- promotionOptions piece to
  ]

pieceDestinations :: GameState -> Position -> Piece -> [Position]
pieceDestinations state from piece =
  case getPieceType piece of
    Pawn -> pawnDestinations state color from
    Knight -> knightDestinations board color from
    Bishop -> slidingDestinations board color from bishopDirections
    Rook -> slidingDestinations board color from rookDirections
    Queen -> slidingDestinations board color from (rookDirections ++ bishopDirections)
    King -> kingDestinations state color from
  where
    board = gsBoard state
    color = getPieceColor piece

pawnDestinations :: GameState -> Color -> Position -> [Position]
pawnDestinations state color from =
  forwardMoves ++ doubleMoves ++ captureMoves ++ enPassantMoves
  where
    board = gsBoard state
    forwardDir = pawnForwardDir color
    startRank = pawnStartRank color
    hasOneStep =
      case movePosition from (0, forwardDir) of
        Just pos | isEmptySquare board pos -> Just pos
        _ -> Nothing
    forwardMoves = maybe [] (\pos -> [pos]) hasOneStep
    doubleMoves =
      case (hasOneStep, movePosition from (0, 2 * forwardDir)) of
        (Just _, Just farPos)
          | rankIndex (posRank from) == startRank && isEmptySquare board farPos ->
            [farPos]
        _ -> []
    captureOffsets = [(-1, forwardDir), (1, forwardDir)]
    captureMoves =
      [ pos
        | offset <- captureOffsets,
          Just pos <- [movePosition from offset],
          isEnemySquare board color pos
      ]
    enPassantMoves =
      case gsEnPassantTarget state of
        Just target
          | isValidEnPassantCapture state color from target -> [target]
        _ -> []

pawnForwardDir :: Color -> Int
pawnForwardDir White = 1
pawnForwardDir Black = -1

pawnStartRank :: Color -> Int
pawnStartRank White = 2
pawnStartRank Black = 7

promotionOptions :: Piece -> Position -> [Maybe PieceType]
promotionOptions piece dest
  | getPieceType piece == Pawn && isFinalRank dest (getPieceColor piece) =
    map Just [Queen, Rook, Bishop, Knight]
  | otherwise = [Nothing]

isFinalRank :: Position -> Color -> Bool
isFinalRank (Position _ (Rank r)) White = r == 8
isFinalRank (Position _ (Rank r)) Black = r == 1

knightDestinations :: Board -> Color -> Position -> [Position]
knightDestinations board color from =
  [ pos
    | offset <- knightOffsets,
      Just pos <- [movePosition from offset],
      not (isFriendlySquare board color pos)
  ]

kingDestinations :: GameState -> Color -> Position -> [Position]
kingDestinations state color from =
  basicMoves ++ castlingMoves
  where
    board = gsBoard state
    basicMoves =
      [ pos
        | offset <- kingOffsets,
          Just pos <- [movePosition from offset],
          not (isFriendlySquare board color pos)
      ]
    castlingMoves = castlingDestinations state color from

castlingDestinations :: GameState -> Color -> Position -> [Position]
castlingDestinations state color from =
  maybeToList (kingsideCastle state color from)
    ++ maybeToList (queensideCastle state color from)

kingsideCastle :: GameState -> Color -> Position -> Maybe Position
kingsideCastle state color from
  | from /= kingStartSquare color = Nothing
  | not (hasKingsideRight (gsCastlingRights state) color) = Nothing
  | boardIsInCheck board color = Nothing
  | any (not . isEmptySquare board) (kingsideEmptySquares color) = Nothing
  | not (noneUnderAttack board color (kingsidePathSquares color)) = Nothing
  | not (hasRookAt board color (rookStartSquare color Kingside)) = Nothing
  | otherwise = Just (kingTargetSquare color Kingside)
  where
    board = gsBoard state

queensideCastle :: GameState -> Color -> Position -> Maybe Position
queensideCastle state color from
  | from /= kingStartSquare color = Nothing
  | not (hasQueensideRight (gsCastlingRights state) color) = Nothing
  | boardIsInCheck board color = Nothing
  | any (not . isEmptySquare board) (queensideEmptySquares color) = Nothing
  | not (noneUnderAttack board color (queensidePathSquares color)) = Nothing
  | not (hasRookAt board color (rookStartSquare color Queenside)) = Nothing
  | otherwise = Just (kingTargetSquare color Queenside)
  where
    board = gsBoard state

kingStartSquare :: Color -> Position
kingStartSquare color = Position (File 5) (Rank (kingRank color))

kingTargetSquare :: Color -> CastleSide -> Position
kingTargetSquare color Kingside = Position (File 7) (Rank (kingRank color))
kingTargetSquare color Queenside = Position (File 3) (Rank (kingRank color))

kingRank :: Color -> Int
kingRank White = 1
kingRank Black = 8

data CastleSide = Kingside | Queenside

kingsideEmptySquares :: Color -> [Position]
kingsideEmptySquares color =
  [Position (File 6) (Rank (kingRank color)), Position (File 7) (Rank (kingRank color))]

kingsidePathSquares :: Color -> [Position]
kingsidePathSquares = kingsideEmptySquares

queensideEmptySquares :: Color -> [Position]
queensideEmptySquares color =
  [ Position (File 4) (Rank (kingRank color)),
    Position (File 3) (Rank (kingRank color)),
    Position (File 2) (Rank (kingRank color))
  ]

queensidePathSquares :: Color -> [Position]
queensidePathSquares color =
  [ Position (File 4) (Rank (kingRank color)),
    Position (File 3) (Rank (kingRank color))
  ]

rookStartSquare :: Color -> CastleSide -> Position
rookStartSquare color Kingside = Position (File 8) (Rank (kingRank color))
rookStartSquare color Queenside = Position (File 1) (Rank (kingRank color))

rookDestinationSquare :: Color -> CastleSide -> Position
rookDestinationSquare color Kingside = Position (File 6) (Rank (kingRank color))
rookDestinationSquare color Queenside = Position (File 4) (Rank (kingRank color))

hasKingsideRight :: CastlingRights -> Color -> Bool
hasKingsideRight rights White = whiteKingside rights
hasKingsideRight rights Black = blackKingside rights

hasQueensideRight :: CastlingRights -> Color -> Bool
hasQueensideRight rights White = whiteQueenside rights
hasQueensideRight rights Black = blackQueenside rights

hasRookAt :: Board -> Color -> Position -> Bool
hasRookAt board color pos =
  case boardPieceAt board pos of
    Just p -> getPieceColor p == color && getPieceType p == Rook
    Nothing -> False

noneUnderAttack :: Board -> Color -> [Position] -> Bool
noneUnderAttack board color positions =
  all (\pos -> not (isSquareUnderAttack board pos (toggleColor color))) positions

slidingDestinations :: Board -> Color -> Position -> [(Int, Int)] -> [Position]
slidingDestinations board color from directions =
  concatMap (slideDirection board color from) directions

slideDirection :: Board -> Color -> Position -> (Int, Int) -> [Position]
slideDirection board color current direction =
  case movePosition current direction of
    Nothing -> []
    Just pos ->
      case boardPieceAt board pos of
        Nothing -> pos : slideDirection board color pos direction
        Just piece
          | isEnemyPiece piece color -> [pos]
          | otherwise -> []

isFriendlySquare :: Board -> Color -> Position -> Bool
isFriendlySquare board color pos =
  case boardPieceAt board pos of
    Just piece -> getPieceColor piece == color
    Nothing -> False

isEnemySquare :: Board -> Color -> Position -> Bool
isEnemySquare board color pos =
  case boardPieceAt board pos of
    Just piece -> getPieceColor piece /= color
    Nothing -> False

isEnemyPiece :: Piece -> Color -> Bool
isEnemyPiece piece color = getPieceColor piece /= color

isEmptySquare :: Board -> Position -> Bool
isEmptySquare board pos =
  case boardPieceAt board pos of
    Just _ -> False
    Nothing -> True

isValidEnPassantCapture :: GameState -> Color -> Position -> Position -> Bool
isValidEnPassantCapture state color from target =
  let fileDiff = abs (fileIndex (posFile target) - fileIndex (posFile from))
      rankDiff = rankIndex (posRank target) - rankIndex (posRank from)
      captureSquare = enPassantCaptureSquare from target
      board = gsBoard state
   in fileDiff == 1
        && rankDiff == pawnForwardDir color
        && isEmptySquare board target
        && isEnemyPawnAt board color captureSquare

isEnemyPawnAt :: Board -> Color -> Position -> Bool
isEnemyPawnAt board color pos =
  case boardPieceAt board pos of
    Just piece -> getPieceColor piece /= color && getPieceType piece == Pawn
    Nothing -> False

enPassantCaptureSquare :: Position -> Position -> Position
enPassantCaptureSquare from target = Position (posFile target) (posRank from)

determineEnPassantTarget :: Piece -> Position -> Position -> Maybe Position
determineEnPassantTarget piece from to
  | getPieceType piece == Pawn
      && abs (rankIndex (posRank to) - rankIndex (posRank from)) == 2 =
    movePosition from (0, pawnForwardDir (getPieceColor piece))
  | otherwise = Nothing
