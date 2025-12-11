module GameState.Types
  ( Result (..),
    GameState (..),
    toggleColor,
  )
where

import Board (Board, CastlingRights)
import Piece (Color)
import Position (Position)

data Result
  = Ongoing
  | Checkmate Color
  | DrawBy50Moves
  | DrawByRepetition
  | Stalemate
  | DrawByAgreement
  deriving (Eq, Show)

data GameState = GameState
  { gsBoard :: Board,
    gsActiveColor :: Color,
    gsCastlingRights :: CastlingRights,
    gsEnPassantTarget :: Maybe Position,
    gsHalfmoveClock :: Int,
    gsFullmoveNumber :: Int,
    gsResult :: Result
  }
  deriving (Eq, Show)

toggleColor :: Color -> Color
toggleColor White = Black
toggleColor Black = White
