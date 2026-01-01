module GameState.StateUpdate
  ( applyMove,
    isLegalMove,
    hasLegalMoves,
    determineResult,
    declareDrawByAgreement,
  )
where

import Control.Monad (guard)
import Data.Maybe (isJust)

import qualified Data.Map.Strict as Map

import Board.Core (Board, CastlingRights (..))
import Board.Query (boardPieceAt, clearCellAt, setPieceAt)
import Move (Move (..))
import Piece (Color (Black, White), Piece (..), PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

import GameState.Types (GameState (..), PositionKey, Result (..), positionKey, toggleColor)
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
      snapshotKey = positionKey nextBoard nextColor nextCastling nextEnPassantTarget
      (updatedCounts, repetitionCount) = incrementPositionCount (gsPositionCounts gs) snapshotKey
      (nextCapturedByWhite, nextCapturedByBlack) =
        recordCapture (gsActiveColor gs) destPiece (gsCapturedByWhite gs, gsCapturedByBlack gs)
      baseState =
        gs
          { gsBoard = nextBoard,
            gsActiveColor = nextColor,
            gsCastlingRights = nextCastling,
            gsEnPassantTarget = nextEnPassantTarget,
            gsHalfmoveClock = nextHalfmove,
            gsFullmoveNumber = nextFullmove,
            gsPositionCounts = updatedCounts,
            gsCapturedByWhite = nextCapturedByWhite,
            gsCapturedByBlack = nextCapturedByBlack,
            gsResult = Ongoing
          }
      nextResult = determineResult baseState nextBoard (gsActiveColor gs) nextColor nextHalfmove repetitionCount
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

determineResult :: GameState -> Board -> Color -> Color -> Int -> Int -> Result
determineResult state nextBoard moverColor nextColor nextHalfmove repetitionCount
  | not opponentHasMoves && opponentInCheck = Checkmate moverColor
  | not opponentHasMoves = Stalemate
  | isInsufficientMaterial nextBoard = DrawByInsufficientMaterial
  | repetitionCount >= 3 = DrawByRepetition
  | nextHalfmove >= 100 = DrawBy50Moves
  | otherwise = Ongoing
  where
    opponentHasMoves = hasLegalMoves state nextColor
    opponentInCheck = boardIsInCheck nextBoard nextColor

isInsufficientMaterial :: Board -> Bool
isInsufficientMaterial board
  | hasMajor white || hasMajor black = False
  | isOnlyKing white && isOnlyKing black = True
  | isOnlyKing white && isSingleMinor black = True
  | isOnlyKing black && isSingleMinor white = True
  | isOnlyKing white && isTwoKnights black = True
  | isOnlyKing black && isTwoKnights white = True
  | isOneMinorEach =
      case (bishopPositions white, knightCount white, bishopPositions black, knightCount black) of
        ([whiteBishop], 0, [blackBishop], 0) -> sameSquareColor whiteBishop blackBishop
        ([], 1, [], 1) -> True
        _ -> False
  | otherwise = False
  where
    white = materialSummary board White
    black = materialSummary board Black
    isOneMinorEach = minorCount white == 1 && minorCount black == 1

data MaterialSummary = MaterialSummary
  { pawnCount :: Int,
    rookCount :: Int,
    queenCount :: Int,
    bishopPositions :: [Position],
    knightCount :: Int
  }

materialSummary :: Board -> Color -> MaterialSummary
materialSummary board color =
  let pieces = piecePositions board color
      pawns = countType Pawn pieces
      rooks = countType Rook pieces
      queens = countType Queen pieces
      bishops = [pos | (pos, piece) <- pieces, getPieceType piece == Bishop]
      knights = countType Knight pieces
   in MaterialSummary
        { pawnCount = pawns,
          rookCount = rooks,
          queenCount = queens,
          bishopPositions = bishops,
          knightCount = knights
        }

countType :: PieceType -> [(Position, Piece)] -> Int
countType pieceType pieces =
  length [() | (_, piece) <- pieces, getPieceType piece == pieceType]

hasMajor :: MaterialSummary -> Bool
hasMajor summary =
  pawnCount summary > 0 || rookCount summary > 0 || queenCount summary > 0

minorCount :: MaterialSummary -> Int
minorCount summary =
  length (bishopPositions summary) + knightCount summary

isOnlyKing :: MaterialSummary -> Bool
isOnlyKing summary = minorCount summary == 0

isSingleMinor :: MaterialSummary -> Bool
isSingleMinor summary = minorCount summary == 1

isTwoKnights :: MaterialSummary -> Bool
isTwoKnights summary = knightCount summary == 2 && null (bishopPositions summary)

sameSquareColor :: Position -> Position -> Bool
sameSquareColor posA posB =
  squareColorIndex posA == squareColorIndex posB

squareColorIndex :: Position -> Int
squareColorIndex pos =
  (fileIndex (posFile pos) + rankIndex (posRank pos)) `mod` 2

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
    2 -> moveRook board color (rookStart color True) (rookDestination color True)
    -2 -> moveRook board color (rookStart color False) (rookDestination color False)
    _ -> board
  where
    color = getPieceColor piece

rookStart :: Color -> Bool -> Position
rookStart color kingside =
  Position
    (File (if kingside then 8 else 1))
    (Rank (kingRank color))

rookDestination :: Color -> Bool -> Position
rookDestination color kingside =
  Position
    (File (if kingside then 6 else 4))
    (Rank (kingRank color))

kingRank :: Color -> Int
kingRank White = 1
kingRank Black = 8

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
updateCastlingRights rights piece fromPosition toPosition captured =
  disableRookCapture
    (disableRookMovement (disableKingRights rights piece) piece fromPosition)
    captured
    toPosition

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

incrementPositionCount :: Map.Map PositionKey Int -> PositionKey -> (Map.Map PositionKey Int, Int)
incrementPositionCount counts key =
  let previousCount = Map.findWithDefault 0 key counts
      newCount = previousCount + 1
   in (Map.insert key newCount counts, newCount)

recordCapture :: Color -> Maybe Piece -> ([Piece], [Piece]) -> ([Piece], [Piece])
recordCapture _ Nothing captures = captures
recordCapture color (Just captured) (whiteCaptures, blackCaptures) =
  case color of
    White -> (whiteCaptures ++ [captured], blackCaptures)
    Black -> (whiteCaptures, blackCaptures ++ [captured])

declareDrawByAgreement :: GameState -> GameState
declareDrawByAgreement gs = gs {gsResult = DrawByAgreement}

applyPromotion :: Piece -> Maybe PieceType -> Piece
applyPromotion piece Nothing = piece
applyPromotion piece (Just newType) = Piece (getPieceColor piece) newType
