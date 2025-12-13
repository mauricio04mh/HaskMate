module UI.Types
  ( UIState (..),
    initialUIState,
    uiSquareSize,
  )
where

import GameState (GameState (..), initialGameState)
import Position (Position)
import UI.Assets (Assets)

data UIState = UIState
  { uiGameState :: GameState,
    uiSelection :: Maybe Position,
    uiPossibleMoves :: [Position],
    uiMessage :: Maybe String
  , uiWindowSize :: (Float, Float),
    uiAssets :: Assets
  }

initialUIState :: Assets -> UIState
initialUIState assets =
      UIState
        { uiGameState = initialGameState,
          uiSelection = Nothing,
          uiPossibleMoves = [],
          uiMessage = Just "Haz clic en una pieza para comenzar",
          uiWindowSize = (640, 680),
          uiAssets = assets
        }

uiSquareSize :: UIState -> Float
uiSquareSize state =
  let (width, height) = uiWindowSize state
      baseSize = min width height / 8
   in max 40 baseSize
