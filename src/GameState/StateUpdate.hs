module GameState.StateUpdate
  ( applyMove,
    isLegalMove,
    hasLegalMoves,
    determineResult,
  )
where

import Control.Monad (guard)
import Data.Maybe (isJust)

import Board (Board, CastlingRights)
import Board.Query (boardPieceAt, clearCellAt, setPieceAt)
import Move (Move (..))
import Piece (Color, Piece (..), PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

import GameState.Types (GameState (..), Result (..), toggleColor)
import GameState.Validation (boardIsInCheck, fileIndex, rankIndex)
import GameState.MoveGen
  ( piecePositions,
    generateMovesForPiece,
    determineEnPassantTarget,
    enPassantCaptureSquare,
    pawnForwardDir,
  )

applyMove :: GameState -> Move -> Maybe GameState
applyMove gs move = do
  (nextBoard, movingPiece, destPiece) <- simulateLegalMove gs move
  let isCapture = case destPiece of
        Just _ -> True
        Nothing -> False
      nextHalfmove =
        if getPieceType movingPiece == Pawn || isCapture
          then 0
          else gsHalfmoveClock gs + 1
      nextFullmove =
        if gsActiveColor gs == Black
          then gsFullmoveNumber gs + 1
          else gsFullmoveNumber gs
      nextCastling =
        updateCastlingRights (gsCastlingRights gs) movingPiece (fromPos move) (toPos move) destPiece
      nextColor = toggleColor (gsActiveColor gs)
      nextEnPassantTarget = determineEnPassantTarget movingPiece (fromPos move) (toPos move)
      baseState =
        gs
          { gsBoard = nextBoard,
            gsActiveColor = nextColor,
            gsCastlingRights = nextCastling,
            gsEnPassantTarget = nextEnPassantTarget,
            gsHalfmoveClock = nextHalfmove,
            gsFullmoveNumber = nextFullmove,
            gsResult = Ongoing
          }
      nextResult = determineResult baseState nextBoard (gsActiveColor gs) nextColor nextHalfmove
  pure baseState {gsResult = nextResult}

simulateLegalMove :: GameState -> Move -> Maybe (Board, Piece, Maybe Piece)
simulateLegalMove gs move = do
  let board = gsBoard gs
  piece <- boardPieceAt board (fromPos move)
  guard (getPieceColor piece == gsActiveColor gs)
  guard (move `elem` generateMovesForPiece gs (fromPos move) piece)
  (nextBoard, destPiece) <- simulateMoveWithPieces board piece move (gsEnPassantTarget gs)
  guard (not (boardIsInCheck nextBoard (gsActiveColor gs)))
  pure (nextBoard, piece, destPiece)

simulateMoveWithPieces :: Board -> Piece -> Move -> Maybe Position -> Maybe (Board, Maybe Piece)
simulateMoveWithPieces board movingPiece move enPassantTarget = do
  let from = fromPos move
      to = toPos move
      destPiece =
        if isEnPassantCapture movingPiece from to enPassantTarget
          then boardPieceAt board (enPassantCaptureSquare from to)
          else boardPieceAt board to
      promotedPiece = applyPromotion movingPiece (promotion move)
      boardAfterCapture =
        if isEnPassantCapture movingPiece from to enPassantTarget
          then clearCellAt board (enPassantCaptureSquare from to)
          else board
      clearedBoard = clearCellAt boardAfterCapture from
      movedKingBoard = setPieceAt clearedBoard to (Just promotedPiece)
      finalBoard =
        if isCastlingMove movingPiece from to
          then moveCastlingRook movedKingBoard movingPiece from to
          else movedKingBoard
  pure (finalBoard, destPiece)

isLegalMove :: GameState -> Move -> Bool
isLegalMove gs = isJust . simulateLegalMove gs

hasLegalMoves :: GameState -> Color -> Bool
hasLegalMoves gs color =
  let board = gsBoard gs
      stateForColor = gs {gsActiveColor = color}
   in any
        (\(from, piece) ->
           any (isLegalMove stateForColor) (generateMovesForPiece stateForColor from piece)
        )
        (piecePositions board color)

determineResult :: GameState -> Board -> Color -> Color -> Int -> Result
determineResult state nextBoard moverColor nextColor nextHalfmove
  | not opponentHasMoves && opponentInCheck = Checkmate moverColor
  | not opponentHasMoves = Stalemate
  | nextHalfmove >= 100 = DrawBy50Moves
  | otherwise = Ongoing
  where
    opponentHasMoves = hasLegalMoves state nextColor
    opponentInCheck = boardIsInCheck nextBoard nextColor

isEnPassantCapture :: Piece -> Position -> Position -> Maybe Position -> Bool
isEnPassantCapture piece from to target =
  case target of
    Just t ->
      getPieceType piece == Pawn
        && to == t
        && abs (fileIndex (posFile t) - fileIndex (posFile from)) == 1
        && rankIndex (posRank t) - rankIndex (posRank from) == pawnForwardDir (getPieceColor piece)
    Nothing -> False

isCastlingMove :: Piece -> Position -> Position -> Bool
isCastlingMove piece from to =
  getPieceType piece == King
    && fileIndex (posFile to) - fileIndex (posFile from) `elem` [2, -2]

moveCastlingRook :: Board -> Piece -> Position -> Position -> Board
moveCastlingRook board piece from to =
  case fileIndex (posFile to) - fileIndex (posFile from) of
    2 -> moveRook board color (rookStartSquare color Kingside) (rookDestinationSquare color Kingside)
    -2 -> moveRook board color (rookStartSquare color Queenside) (rookDestinationSquare color Queenside)
    _ -> board
  where
    color = getPieceColor piece

moveRook :: Board -> Color -> Position -> Position -> Board
moveRook board color fromRook toRook =
  case boardPieceAt board fromRook of
    Just rookPiece
      | getPieceColor rookPiece == color && getPieceType rookPiece == Rook ->
        setPieceAt (clearCellAt board fromRook) toRook (Just rookPiece)
    _ -> board

updateCastlingRights ::
  CastlingRights ->
  Piece ->
  Position ->
  Position ->
  Maybe Piece ->
  CastlingRights
updateCastlingRights rights piece fromPos toPos captured =
  disableRookCapture
    (disableRookMovement (disableKingRights rights piece) piece fromPos)
    captured
    toPos

disableKingRights :: CastlingRights -> Piece -> CastlingRights
disableKingRights rights piece
  | getPieceType piece == King =
    case getPieceColor piece of
      White -> rights {whiteKingside = False, whiteQueenside = False}
      Black -> rights {blackKingside = False, blackQueenside = False}
  | otherwise = rights

disableRookMovement :: CastlingRights -> Piece -> Position -> CastlingRights
disableRookMovement rights piece pos
  | getPieceType piece /= Rook = rights
  | otherwise = case getPieceColor piece of
    White -> case pos of
      Position (File 1) (Rank 1) -> rights {whiteQueenside = False}
      Position (File 8) (Rank 1) -> rights {whiteKingside = False}
      _ -> rights
    Black -> case pos of
      Position (File 1) (Rank 8) -> rights {blackQueenside = False}
      Position (File 8) (Rank 8) -> rights {blackKingside = False}
      _ -> rights

disableRookCapture :: CastlingRights -> Maybe Piece -> Position -> CastlingRights
disableRookCapture rights (Just capturedPiece) pos
  | getPieceType capturedPiece /= Rook = rights
  | otherwise = disableRookMovement rights capturedPiece pos
disableRookCapture rights Nothing _ = rights

applyPromotion :: Piece -> Maybe PieceType -> Piece
applyPromotion piece Nothing = piece
applyPromotion piece (Just newType) = Piece (getPieceColor piece) newType
