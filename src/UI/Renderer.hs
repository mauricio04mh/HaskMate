module UI.Renderer
  ( drawUI,
  )
where

import Board.Core (Board, cellPiece, cellPosition)
import Data.Maybe (fromMaybe)
import GameState (gsBoard)
import Graphics.Gloss
  ( Picture,
    circleSolid,
    color,
    makeColorI,
    pictures,
    rectangleSolid,
    scale,
    text,
    translate,
  )
import qualified Graphics.Gloss as Gloss (Color)
import Piece (Piece (..))
import Position (Position)
import UI.Assets (Assets, pieceSprite)
import UI.Coordinates (positionToPoint)
import UI.Message (formatStateMessage)
import UI.Types (UIScreen (..), UIState (..), uiSquareSize)

drawUI :: UIState -> Picture
drawUI state =
  case uiScreen state of
    Menu -> drawMenu state
    Playing -> drawGame state

drawGame :: UIState -> Picture
drawGame state =
  let sqSize = uiSquareSize state
      gameState = uiGameState state
   in pictures
        [ drawBoard sqSize,
          drawHighlights sqSize state,
          drawPieces sqSize (uiAssets state) (gsBoard gameState),
          drawMessage state
        ]

drawMenu :: UIState -> Picture
drawMenu state =
  let (width, height) = uiWindowSize state
      background =
        color (makeColorI 24 26 27 255) $
          rectangleSolid width height
      startY = height / 4
      lineSpacing = 40
      linesWithIndex = zip menuLines [0 :: Int ..]
      textPictures =
        [ translate (-width / 2 + 40) (startY - fromIntegral idx * lineSpacing) $
            scale lineScale lineScale $
              color (makeColorI 255 255 255 255) $
                text line
          | (line, idx) <- linesWithIndex
        ]
      lineScale = 0.2
   in pictures (background : textPictures)

menuLines :: [String]
menuLines =
  [ "HaskMate",
    "",
    "Selecciona un modo:",
    "1: Humano vs Humano",
    "2: Humano (Blancas) vs IA (Negras)",
    "3: IA (Blancas) vs Humano (Negras)",
    "4: IA vs IA",
    "",
    "Controles durante la partida:",
    "Esc: volver al menu",
    "R: reiniciar partida",
    "Espacio: pausa (solo IA vs IA)"
  ]

drawBoard :: Float -> Picture
drawBoard sqSize =
  pictures
    [ translate x y $
        color (squareColor file rank) $
          rectangleSolid sqSize sqSize
      | file <- [1 .. 8],
        rank <- [1 .. 8],
        let x = (fromIntegral file - 4.5) * sqSize,
        let y = (fromIntegral rank - 4.5) * sqSize
    ]

squareColor :: Int -> Int -> Gloss.Color
squareColor file rank
  | even (file + rank) = makeColorI 240 217 181 255
  | otherwise = makeColorI 181 136 99 255

drawHighlights :: Float -> UIState -> Picture
drawHighlights sqSize state =
  pictures $
    maybe [] (\pos -> [highlightSelection sqSize pos]) (uiSelection state)
      ++ map (highlightMove sqSize) (uiPossibleMoves state)

highlightSelection :: Float -> Position -> Picture
highlightSelection sqSize pos =
  highlightCircle sqSize pos (makeColorI 255 255 0 100)

highlightMove :: Float -> Position -> Picture
highlightMove sqSize pos =
  highlightCircle sqSize pos (makeColorI 0 255 0 120)

highlightCircle :: Float -> Position -> Gloss.Color -> Picture
highlightCircle sqSize pos highlightColor =
  let (x, y) = positionToPoint sqSize pos
   in translate x y $ color highlightColor $ circleSolid (sqSize / 4)

drawPieces :: Float -> Assets -> Board -> Picture
drawPieces sqSize assets board =
  pictures
    [ drawPiece sqSize assets (cellPosition cell) piece
      | row <- board,
        cell <- row,
        Just piece <- [cellPiece cell]
    ]

drawPiece :: Float -> Assets -> Position -> Piece -> Picture
drawPiece sqSize assets pos piece =
  let (x, y) = positionToPoint sqSize pos
      (sprite, spriteSize) = pieceSprite assets piece
      scaleFactor = (sqSize * 0.85) / spriteSize
   in translate x y $ scale scaleFactor scaleFactor sprite

drawMessage :: UIState -> Picture
drawMessage state =
  let (width, height) = uiWindowSize state
      x = -width / 2 + 20
      y = -height / 2 + 20
   in translate x y $
        scale 0.15 0.15 $
          color (makeColorI 255 255 255 255) $
            text (fromMaybe (formatStateMessage (uiGameState state)) (uiMessage state))
