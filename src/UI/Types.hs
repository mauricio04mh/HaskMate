module UI.Types
  ( UIState (..),
    UIScreen (..),
    Controller (..),
    initialUIState,
    uiSquareSize,
    defaultAICooldown,
    controllerForTurn,
    isHumanTurn,
    isAITurn,
  )
where

import GameState (GameState (..), initialGameState)
import Piece (Color (Black, White))
import Position (Position)
import UI.Assets (Assets)

data UIState = UIState
  { uiGameState :: GameState,
    uiSelection :: Maybe Position,
    uiPossibleMoves :: [Position],
    uiMessage :: Maybe String,
    uiWindowSize :: (Float, Float),
    uiAssets :: Assets,
    uiScreen :: UIScreen,
    uiWhiteController :: Controller,
    uiBlackController :: Controller,
    uiIsThinking :: Bool,
    uiAICooldown :: Float,
    uiPaused :: Bool
  }

data UIScreen = Menu | Playing
  deriving (Eq, Show)

data Controller
  = Human
  | AI Int
  deriving (Eq, Show)

initialUIState :: Assets -> UIState
initialUIState assets =
  UIState
    { uiGameState = initialGameState,
      uiSelection = Nothing,
      uiPossibleMoves = [],
      uiMessage = Just "Haz clic en una pieza para comenzar",
      uiWindowSize = (640, 680),
      uiAssets = assets,
      uiScreen = Menu,
      uiWhiteController = Human,
      uiBlackController = AI 3,
      uiIsThinking = False,
      uiAICooldown = defaultAICooldown,
      uiPaused = False
    }

defaultAICooldown :: Float
defaultAICooldown = 2.0

uiSquareSize :: UIState -> Float
uiSquareSize state =
  let (width, height) = uiWindowSize state
      baseSize = min width height / 8
   in max 40 baseSize

controllerForTurn :: UIState -> Controller
controllerForTurn state =
  case gsActiveColor (uiGameState state) of
    White -> uiWhiteController state
    Black -> uiBlackController state

isHumanTurn :: UIState -> Bool
isHumanTurn state =
  case controllerForTurn state of
    Human -> True
    _ -> False

isAITurn :: UIState -> Bool
isAITurn state =
  case controllerForTurn state of
    AI _ -> True
    _ -> False
