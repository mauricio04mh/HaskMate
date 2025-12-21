module UI.Controller
  ( handleEvent,
    stepSimulation,
  )
where

import AI.Search (minimax)
import Board.Query (boardPieceAt)
import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (listToMaybe)
import GameState
  ( GameState,
    Result (..),
    applyMove,
    declareDrawByAgreement,
    gsActiveColor,
    gsBoard,
    gsResult,
    isLegalMove,
  )
import GameState.MoveGen (generateMovesForPiece)
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), KeyState (..), MouseButton (..), SpecialKey (..))
import Move (Move (..))
import Piece (Piece, PieceType (..), getPieceColor)
import Position (Position)
import UI.Coordinates (screenToPosition)
import UI.Message (formatStateMessage)
import UI.Types
  ( Controller (..),
    UIScreen (..),
    UIState (..),
    controllerForTurn,
    defaultAICooldown,
    initialUIState,
    isAITurn,
    isHumanTurn,
    uiSquareSize,
  )

handleEvent :: Event -> UIState -> UIState
handleEvent (EventResize (w, h)) state =
  state {uiWindowSize = (fromIntegral w, fromIntegral h)}
handleEvent event state =
  case uiScreen state of
    Menu -> handleMenuEvent event state
    Playing -> handlePlayingEvent event state

handleMenuEvent :: Event -> UIState -> UIState
handleMenuEvent (EventKey (Char key) Down _ _) state =
  case controllersForOption key of
    Just (whiteCtrl, blackCtrl) -> startGame whiteCtrl blackCtrl state
    Nothing -> state
handleMenuEvent _ state = state

handlePlayingEvent :: Event -> UIState -> UIState
handlePlayingEvent (EventKey (Char 'r') Down _ _) state = restartCurrentMode state
handlePlayingEvent (EventKey (Char 't') Down _ _) state =
  let newGameState = declareDrawByAgreement (uiGameState state)
   in applyGameStateChange newGameState state
handlePlayingEvent (EventKey (SpecialKey KeyEsc) Down _ _) state = returnToMenu state
handlePlayingEvent (EventKey (SpecialKey KeySpace) Down _ _) state = togglePause state
handlePlayingEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state
  | not (isHumanTurn state) =
      state
        { uiSelection = Nothing,
          uiPossibleMoves = [],
          uiMessage = Just (aiTurnPrompt state)
        }
  | otherwise =
      let sqSize = uiSquareSize state
       in maybe state (`handleBoardClick` state) (screenToPosition sqSize mousePos)
handlePlayingEvent _ state = state

stepSimulation :: Float -> UIState -> UIState
stepSimulation dt state
  | uiScreen state /= Playing = state {uiIsThinking = False}
  | uiPaused state = state {uiIsThinking = False}
  | gsResult (uiGameState state) /= Ongoing = state {uiIsThinking = False}
  | not (isAITurn state) = state {uiIsThinking = False}
  | otherwise =
      let updatedCooldown = max 0 (uiAICooldown state - dt)
          waitingState = state {uiAICooldown = updatedCooldown, uiIsThinking = True}
       in if updatedCooldown > 0
            then waitingState
            else performAIMove waitingState

handleBoardClick :: Position -> UIState -> UIState
handleBoardClick pos state =
  case uiSelection state of
    Nothing -> selectPiece pos state
    Just selected
      | selected == pos -> state {uiSelection = Nothing, uiPossibleMoves = []}
      | otherwise ->
          case boardPieceAt (gsBoard gameState) pos of
            Just piece
              | getPieceColor piece == gsActiveColor gameState -> selectPiece pos state
            _ -> attemptMove selected pos state
  where
    gameState = uiGameState state

selectPiece :: Position -> UIState -> UIState
selectPiece pos state =
  case boardPieceAt board pos of
    Just piece
      | getPieceColor piece == gsActiveColor gameState ->
          let legalMoves =
                [ toPos move
                  | move <- generateMovesForPiece gameState pos piece,
                    isLegalMove gameState move
                ]
           in state
                { uiSelection = Just pos,
                  uiPossibleMoves = legalMoves,
                  uiMessage = Just (formatStateMessage gameState)
                }
    _ -> state {uiSelection = Nothing, uiPossibleMoves = [], uiMessage = Just (formatStateMessage gameState)}
  where
    gameState = uiGameState state
    board = gsBoard gameState

attemptMove :: Position -> Position -> UIState -> UIState
attemptMove from to state =
  case boardPieceAt board from of
    Just piece ->
      case selectMoveForDestination gameState from to piece of
        Just move
          | isLegalMove gameState move ->
              case applyMove gameState move of
                Just newGame -> applyGameStateChange newGame state
                Nothing -> state {uiMessage = Just "No se pudo aplicar el movimiento"}
          | otherwise ->
              state {uiMessage = Just "Movimiento ilegal"}
        Nothing -> state {uiMessage = Just "Movimiento inválido"}
    Nothing -> state {uiMessage = Just (formatStateMessage gameState)}
  where
    gameState = uiGameState state
    board = gsBoard gameState

selectMoveForDestination :: GameState -> Position -> Position -> Piece -> Maybe Move
selectMoveForDestination gameState from to piece =
  chooseMoveByPriority matches
  where
    matches =
      filter
        (\move -> toPos move == to)
        (generateMovesForPiece gameState from piece)

chooseMoveByPriority :: [Move] -> Maybe Move
chooseMoveByPriority moves =
  find (\m -> promotion m == Just Queen) moves
    <|> find (\m -> promotion m == Nothing) moves
    <|> listToMaybe moves

performAIMove :: UIState -> UIState
performAIMove state =
  case controllerForTurn state of
    AI depth ->
      case minimax depth (uiGameState state) of
        Nothing ->
          state
            { uiMessage = Just "IA sin movimientos",
              uiIsThinking = False,
              uiAICooldown = defaultAICooldown
            }
        Just move ->
          case applyMove (uiGameState state) move of
            Nothing ->
              state
                { uiMessage = Just "Error: applyMove falló para la IA",
                  uiIsThinking = False,
                  uiAICooldown = defaultAICooldown
                }
            Just newGame -> applyGameStateChange newGame state
    Human -> state {uiIsThinking = False}

applyGameStateChange :: GameState -> UIState -> UIState
applyGameStateChange newGame state =
  let cleared =
        state
          { uiGameState = newGame,
            uiSelection = Nothing,
            uiPossibleMoves = []
          }
   in updateMessageForTurn cleared

updateMessageForTurn :: UIState -> UIState
updateMessageForTurn state =
  let gameState = uiGameState state
      ongoing = gsResult gameState == Ongoing
      aiTurnActive = ongoing && isAITurn state
      message
        | not ongoing = Just (formatStateMessage gameState)
        | aiTurnActive = Just (aiTurnPrompt state)
        | otherwise = Just (formatStateMessage gameState)
      nextCooldown =
        if aiTurnActive
          then defaultAICooldown
          else uiAICooldown state
   in state
        { uiMessage = message,
          uiIsThinking = aiTurnActive,
          uiAICooldown = nextCooldown
        }

restartCurrentMode :: UIState -> UIState
restartCurrentMode state =
  startGame (uiWhiteController state) (uiBlackController state) state

startGame :: Controller -> Controller -> UIState -> UIState
startGame whiteCtrl blackCtrl state =
  let base = initialUIState (uiAssets state)
      prepared =
        base
          { uiWindowSize = uiWindowSize state,
            uiWhiteController = whiteCtrl,
            uiBlackController = blackCtrl,
            uiScreen = Playing,
            uiAICooldown = defaultAICooldown,
            uiPaused = False
          }
   in updateMessageForTurn prepared

returnToMenu :: UIState -> UIState
returnToMenu state =
  state
    { uiScreen = Menu,
      uiSelection = Nothing,
      uiPossibleMoves = [],
      uiIsThinking = False,
      uiAICooldown = defaultAICooldown,
      uiPaused = False,
      uiMessage = Just "Selecciona un modo con las teclas 1-4"
    }

togglePause :: UIState -> UIState
togglePause state
  | not (bothAIControllers state) = state
  | otherwise =
      let newPaused = not (uiPaused state)
          baseState =
            state
              { uiPaused = newPaused,
                uiIsThinking = False,
                uiAICooldown = defaultAICooldown
              }
          message
            | newPaused = Just "IA vs IA en pausa (Espacio para reanudar)"
            | isAITurn baseState = Just (aiTurnPrompt baseState)
            | otherwise = Just (formatStateMessage (uiGameState baseState))
       in baseState {uiMessage = message}

bothAIControllers :: UIState -> Bool
bothAIControllers state =
  isAIController (uiWhiteController state) && isAIController (uiBlackController state)

isAIController :: Controller -> Bool
isAIController (AI _) = True
isAIController _ = False

controllersForOption :: Char -> Maybe (Controller, Controller)
controllersForOption '1' = Just (Human, Human)
controllersForOption '2' = Just (Human, AI 3)
controllersForOption '3' = Just (AI 3, Human)
controllersForOption '4' = Just (AI 3, AI 3)
controllersForOption _ = Nothing

aiTurnPrompt :: UIState -> String
aiTurnPrompt state =
  "Turno de la IA (" ++ show (gsActiveColor (uiGameState state)) ++ ")..."
