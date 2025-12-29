module GameState.Types
  ( Result (..),
    GameState (..),
    PositionKey,
    positionKey,
    toggleColor,
    initialGameState,
  )
where

import qualified Data.Map.Strict as Map

import Board.Core (Board, CastlingRights (..), emptyBoard)
import Piece (Color (Black, White), Piece)
import Position (Position)

data Result
  = Ongoing
  | Checkmate Color
  | DrawBy50Moves
  | DrawByRepetition
  | Stalemate
  | DrawByAgreement
  deriving (Eq, Show)

type PositionKey = (Board, Color, CastlingRights, Maybe Position)

data GameState = GameState
  { gsBoard :: Board,
    gsActiveColor :: Color,
    gsCastlingRights :: CastlingRights,
    gsEnPassantTarget :: Maybe Position,
    gsHalfmoveClock :: Int,
    gsFullmoveNumber :: Int,
    gsResult :: Result,
    gsPositionCounts :: Map.Map PositionKey Int,
    gsCapturedByWhite :: [Piece],
    gsCapturedByBlack :: [Piece]
  }
  deriving (Eq, Show)

toggleColor :: Color -> Color
toggleColor White = Black
toggleColor Black = White

initialCastlingRights :: CastlingRights
initialCastlingRights = CastlingRights True True True True

initialGameState :: GameState
initialGameState =
  let baseState =
        GameState
          { gsBoard = emptyBoard,
            gsActiveColor = White,
            gsCastlingRights = initialCastlingRights,
            gsEnPassantTarget = Nothing,
            gsHalfmoveClock = 0,
            gsFullmoveNumber = 1,
            gsResult = Ongoing,
            gsPositionCounts = Map.empty,
            gsCapturedByWhite = [],
            gsCapturedByBlack = []
          }
      startKey =
        positionKey
          (gsBoard baseState)
          (gsActiveColor baseState)
          (gsCastlingRights baseState)
          (gsEnPassantTarget baseState)
   in baseState {gsPositionCounts = Map.singleton startKey 1}

positionKey :: Board -> Color -> CastlingRights -> Maybe Position -> PositionKey
positionKey board color rights enPassant =
  (board, color, rights, enPassant)
