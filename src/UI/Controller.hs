module UI.Controller
  ( handleEvent,
    stepSimulation,
  )
where

import AI.Search (minimaxWithStats)
import AI.SearchStats (SearchStats (..))
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
    isKingInCheck,
    isLegalMove,
    toggleColor,
  )
import GameState.MoveGen (generateMovesForPiece)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), MouseButton (..), SpecialKey (..))
import Move (Move (..))
import Piece (Piece, PieceType (..), getPieceColor)
import Position (File (..), Position (..), Rank (..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
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

handleEvent :: Event -> UIState -> IO UIState
handleEvent (EventResize (w, h)) state =
  pure $ state {uiWindowSize = (fromIntegral w, fromIntegral h)}
handleEvent event state =
  case uiScreen state of
    Menu -> pure $ handleMenuEvent event state
    Playing -> pure $ handlePlayingEvent event state

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

stepSimulation :: Float -> UIState -> IO UIState
stepSimulation dt state
  | uiScreen state /= Playing = pure $ state {uiIsThinking = False}
  | uiPaused state = pure $ state {uiIsThinking = False}
  | gsResult (uiGameState state) /= Ongoing = pure $ state {uiIsThinking = False}
  | not (isAITurn state) = pure $ state {uiIsThinking = False}
  | otherwise =
      let updatedCooldown = max 0 (uiAICooldown state - dt)
          waitingState = state {uiAICooldown = updatedCooldown, uiIsThinking = True}
       in if updatedCooldown > 0
            then pure waitingState
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

performAIMove :: UIState -> IO UIState
performAIMove state =
  case controllerForTurn state of
    AI depth -> do
      start <- getCPUTime
      (mMove, stats) <- minimaxWithStats depth (uiGameState state)
      end <- getCPUTime
      let thinkSeconds = realToFrac (max 0 (end - start)) / 1e12 :: Float
          nextCooldown = defaultAICooldown + thinkSeconds
      case mMove of
        Nothing ->
          let active = gsActiveColor (uiGameState state)
              board = gsBoard (uiGameState state)
              result =
                if isKingInCheck board active
                  then Checkmate (toggleColor active)
                  else Stalemate
              newGame = (uiGameState state) {gsResult = result}
           in pure
                state
                  { uiGameState = newGame,
                    uiMessage = Just (formatStateMessage newGame),
                    uiIsThinking = False,
                    uiAICooldown = nextCooldown
                  }
        Just move ->
          case applyMove (uiGameState state) move of
            Nothing ->
              pure
                state
                  { uiMessage = Just "Error: applyMove falló para la IA",
                    uiIsThinking = False,
                    uiAICooldown = nextCooldown
                  }
            Just newGame ->
              let cleared =
                    state
                      { uiGameState = newGame,
                        uiSelection = Nothing,
                        uiPossibleMoves = []
                      }
                  msg =
                    case gsResult newGame of
                      Ongoing -> formatAIMoveMessage move stats
                      _ -> formatStateMessage newGame
               in pure $ cleared {uiMessage = Just msg, uiIsThinking = False, uiAICooldown = nextCooldown}
    Human -> pure $ state {uiIsThinking = False}

formatAIMoveMessage :: Move -> SearchStats -> String
formatAIMoveMessage move stats =
  printf "IA movio %s (%d nodos, %.6fs)" (renderMove move) (nodesVisited stats) (fromIntegral (elapsedPicos stats) / 1e12 :: Double)

renderMove :: Move -> String
renderMove (Move (Position (File f1) (Rank r1)) (Position (File f2) (Rank r2)) promo) =
  [fileToChar f1, rankToChar r1, fileToChar f2, rankToChar r2] ++ promoChar promo
  where
    fileToChar f = "abcdefgh" !! (f - 1)
    rankToChar r = "12345678" !! (r - 1)
    promoChar Nothing = ""
    promoChar (Just Queen) = "q"
    promoChar (Just Rook) = "r"
    promoChar (Just Bishop) = "b"
    promoChar (Just Knight) = "n"
    promoChar (Just _) = ""

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
controllersForOption '2' = Just (Human, AI 7)
controllersForOption '3' = Just (AI 7, Human)
controllersForOption '4' = Just (AI 6, AI 1)
controllersForOption _ = Nothing

aiTurnPrompt :: UIState -> String
aiTurnPrompt state =
  "Turno de la IA (" ++ show (gsActiveColor (uiGameState state)) ++ ")..."
