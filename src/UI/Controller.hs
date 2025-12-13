module UI.Controller
  ( handleEvent,
    stepSimulation,
  )
where

import Control.Applicative ((<|>))
import Data.List (find)
import Data.Maybe (listToMaybe)

import Board.Query (boardPieceAt)
import GameState (GameState, applyMove, declareDrawByAgreement, isLegalMove, gsActiveColor, gsBoard)
import GameState.MoveGen (generateMovesForPiece)
import Move (Move (..))
import Piece (Piece, PieceType (..), getPieceColor)
import Position (Position)
import Graphics.Gloss.Interface.Pure.Game (Event (..), Key (..), KeyState (..), MouseButton (..))

import UI.Coordinates (screenToPosition)
import UI.Message (formatStateMessage)
import UI.Types (initialUIState, UIState (..), uiSquareSize)

handleEvent :: Event -> UIState -> UIState
handleEvent (EventKey (Char 'r') Down _ _) state =
  let resetState = initialUIState (uiAssets state)
   in resetState {uiWindowSize = uiWindowSize state}
handleEvent (EventKey (Char 't') Down _ _) state =
  let newGameState = declareDrawByAgreement (uiGameState state)
   in state
        { uiGameState = newGameState,
          uiSelection = Nothing,
          uiPossibleMoves = [],
          uiMessage = Just (formatStateMessage newGameState)
        }
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state =
  let sqSize = uiSquareSize state
   in maybe state (`handleBoardClick` state) (screenToPosition sqSize mousePos)
handleEvent (EventResize (w, h)) state =
  state {uiWindowSize = (fromIntegral w, fromIntegral h)}
handleEvent _ state = state

stepSimulation :: Float -> UIState -> UIState
stepSimulation _ = id

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
              Just newGame ->
                state
                  { uiGameState = newGame,
                    uiSelection = Nothing,
                    uiPossibleMoves = [],
                    uiMessage = Just (formatStateMessage newGame)
                  }
              Nothing ->
                state {uiMessage = Just "No se pudo aplicar el movimiento"}
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
