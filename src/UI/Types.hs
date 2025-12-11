module UI.Types
  ( UIState (..),
    initialUIState,
  )
where

import GameState (GameState (..), initialGameState)
import Position (Position)

data UIState = UIState
  { uiGameState :: GameState,
    uiSelection :: Maybe Position,
    uiPossibleMoves :: [Position],
    uiMessage :: Maybe String
  }

initialUIState :: UIState
initialUIState =
  UIState
    { uiGameState = initialGameState,
      uiSelection = Nothing,
      uiPossibleMoves = [],
      uiMessage = Just "Haz clic en una pieza para comenzar",
    }
