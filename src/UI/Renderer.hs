module UI.Renderer
  ( drawUI,
  )
where

import Data.Maybe (fromMaybe)

import Board.Core (Board, cellPiece, cellPosition)
import GameState (gsBoard)
import Piece (Color (..), Piece (..), PieceType (..))
import Graphics.Gloss
  ( Color,
    Picture,
    circleSolid,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
  )
import Position (Position)
import UI.Coordinates (positionToPoint, squareSize)
import UI.Message (formatStateMessage)
import UI.Types (UIState (..))

drawUI :: UIState -> Picture
drawUI state =
  pictures
    [ drawBoard,
      drawHighlights state,
      drawPieces (gsBoard gameState),
      drawMessage state
    ]
  where
    gameState = uiGameState state

drawBoard :: Picture
drawBoard =
  pictures
    [ translate x y $
      color (squareColor file rank) $
      rectangleSolid squareSize squareSize
    | file <- [1 .. 8],
      rank <- [1 .. 8],
      let x = (fromIntegral file - 4.5) * squareSize,
      let y = (fromIntegral rank - 4.5) * squareSize
    ]

squareColor :: Int -> Int -> Color
squareColor file rank
  | even (file + rank) = makeColorI 240 217 181 255
  | otherwise = makeColorI 181 136 99 255

drawHighlights :: UIState -> Picture
drawHighlights state =
  pictures $
    maybe [] (\pos -> [highlightSelection pos]) (uiSelection state)
      ++ map highlightMove (uiPossibleMoves state)

highlightSelection :: Position -> Picture
highlightSelection pos =
  highlightCircle pos (makeColorI 255 255 0 100)

highlightMove :: Position -> Picture
highlightMove pos =
  highlightCircle pos (makeColorI 0 255 0 120)

highlightCircle :: Position -> Color -> Picture
highlightCircle pos highlightColor =
  let (x, y) = positionToPoint pos
   in translate x y $ color highlightColor $ circleSolid (squareSize / 4)

drawPieces :: Board -> Picture
drawPieces board =
  pictures
    [ translate x y $
      color (pieceColor piece) $
      scale 0.25 0.25 $
      text (pieceSymbol piece)
    | row <- board,
      cell <- row,
      Just piece <- [cellPiece cell],
      let (x, y) = positionToPoint (cellPosition cell)
    ]

pieceSymbol :: Piece -> String
pieceSymbol (Piece White pieceType) = whiteSymbol pieceType
pieceSymbol (Piece Black pieceType) = blackSymbol pieceType

whiteSymbol, blackSymbol :: PieceType -> String
whiteSymbol King = "♔"
whiteSymbol Queen = "♕"
whiteSymbol Rook = "♖"
whiteSymbol Bishop = "♗"
whiteSymbol Knight = "♘"
whiteSymbol Pawn = "♙"
blackSymbol King = "♚"
blackSymbol Queen = "♛"
blackSymbol Rook = "♜"
blackSymbol Bishop = "♝"
blackSymbol Knight = "♞"
blackSymbol Pawn = "♟"

pieceColor :: Piece -> Color
pieceColor (Piece White _) = makeColorI 255 255 255 255
pieceColor (Piece Black _) = makeColorI 0 0 0 255

drawMessage :: UIState -> Picture
drawMessage state =
  translate (-300) (-320) $
  scale 0.15 0.15 $
  color (makeColorI 255 255 255 255) $
  text (fromMaybe (formatStateMessage (uiGameState state)) (uiMessage state))
