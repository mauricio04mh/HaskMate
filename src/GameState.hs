module GameState where

import Board (Board, CastlingRights)
import Board.Query (boardPieceAt, clearCellAt, setPieceAt)
import Move (Move (..))
import Piece (Color, Piece (..), PieceType (..), getPieceColor, getPieceType)
import Position (File (..), Position (..), Rank (..))

data Result
  = Ongoing
  | Checkmate Color
  | DrawBy50Moves
  deriving (Eq, Show)

data GameState = GameState
  { gsBoard :: Board,
    gsActiveColor :: Color,
    gsCastlingRights :: CastlingRights,
    gsHalfmoveClock :: Int, -- Contador de movimientos desde la última captura o movimiento de peón.
    gsFullmoveNumber :: Int, -- Número de movimiento completo.
    gsResult :: Result
  }
  deriving (Eq, Show)

-- TODO: AGREGAR EN RESULT DRAWBYREPEITION, STALEMATE, DRAWBYAGREEMENT

applyMove :: GameState -> Move -> Maybe GameState
applyMove gs move = do
  movingPiece <- boardPieceAt (gsBoard gs) (fromPos move)
  let destPiece = boardPieceAt (gsBoard gs) (toPos move)
      promotedPiece = applyPromotion movingPiece (promotion move)
      clearedBoard = clearCellAt (gsBoard gs) (fromPos move)
      nextBoard = setPieceAt clearedBoard (toPos move) (Just promotedPiece)
      isCapture = case destPiece of
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
   in Just
        gs
          { gsBoard = nextBoard,
            gsActiveColor = nextColor,
            gsCastlingRights = nextCastling,
            gsHalfmoveClock = nextHalfmove,
            gsFullmoveNumber = nextFullmove
          }

applyPromotion :: Piece -> Maybe PieceType -> Piece
applyPromotion piece Nothing = piece
applyPromotion piece (Just newType) = Piece (getPieceColor piece) newType

toggleColor :: Color -> Color
toggleColor White = Black
toggleColor Black = White

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
