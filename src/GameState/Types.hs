module GameState.Types
  ( Result (..),
    GameState (..),
    toggleColor,
    initialGameState,
  )
where

import Board (Board, CastlingRights, emptyBoard)
import Piece (Color (Black, White))
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

initialCastlingRights :: CastlingRights
initialCastlingRights = CastlingRights True True True True

initialGameState :: GameState
initialGameState =
  GameState
    { gsBoard = emptyBoard,
      gsActiveColor = White,
      gsCastlingRights = initialCastlingRights,
      gsEnPassantTarget = Nothing,
      gsHalfmoveClock = 0,
      gsFullmoveNumber = 1,
      gsResult = Ongoing
    }
