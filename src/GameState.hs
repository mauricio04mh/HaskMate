module GameState where

import Board (Board, CastlingRights)
import Piece (Color)

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
