module UI.Renderer
  ( drawUI,
  )
where

import Board.Core (Board, cellPiece, cellPosition)
import Data.Maybe (fromMaybe)
import GameState (gsBoard, gsCapturedByBlack, gsCapturedByWhite)
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
          drawGraveyards state,
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

data GraveyardStyle = GraveyardStyle
  { gyLabel :: String,
    gyPanelColor :: Gloss.Color,
    gyPanelBorder :: Gloss.Color,
    gySignColor :: Gloss.Color,
    gySignTextColor :: Gloss.Color,
    gySlotColor :: Gloss.Color
  }

drawGraveyards :: UIState -> Picture
drawGraveyards state =
  let sqSize = uiSquareSize state
      (winW, winH) = uiWindowSize state
      boardSize = sqSize * 8
      boardHalf = boardSize / 2
      sideWidth = max 0 ((winW - boardSize) / 2)
      iconSize = max 18 (sqSize * 0.4)
      padding = iconSize * 0.2
      panelWidth = sideWidth
      panelHeight = winH
      panelY = 0
      signHeight = iconSize * 0.7
      signY = panelY + panelHeight / 2 - signHeight / 2 - 6
      startY = signY - signHeight / 2 - padding - iconSize / 2
      leftX = -boardHalf - panelWidth / 2
      rightX = boardHalf + panelWidth / 2
      gameState = uiGameState state
      blackLost = gsCapturedByWhite gameState
      whiteLost = gsCapturedByBlack gameState
      whiteStyle =
        GraveyardStyle
          { gyLabel = "Cementerio Blancas",
            gyPanelColor = makeColorI 50 50 55 220,
            gyPanelBorder = makeColorI 20 20 25 230,
            gySignColor = makeColorI 230 230 235 255,
            gySignTextColor = makeColorI 30 30 35 255,
            gySlotColor = makeColorI 80 80 90 200
          }
      blackStyle =
        GraveyardStyle
          { gyLabel = "Cementerio Negras",
            gyPanelColor = makeColorI 235 230 220 230,
            gyPanelBorder = makeColorI 165 155 145 240,
            gySignColor = makeColorI 40 40 45 255,
            gySignTextColor = makeColorI 235 235 235 255,
            gySlotColor = makeColorI 210 200 190 220
          }
   in if panelWidth <= 0
        then pictures []
        else
          pictures
            [ drawGraveyardColumn (uiAssets state) whiteStyle whiteLost leftX panelY panelWidth panelHeight signY signHeight startY iconSize padding,
              drawGraveyardColumn (uiAssets state) blackStyle blackLost rightX panelY panelWidth panelHeight signY signHeight startY iconSize padding
            ]

drawGraveyardColumn ::
  Assets ->
  GraveyardStyle ->
  [Piece] ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Picture
drawGraveyardColumn assets style pieces x panelY panelWidth panelHeight signY signHeight startY iconSize padding =
  let panelPic = drawGraveyardPanel style x panelY panelWidth panelHeight
      signPic = drawGraveyardSign style x signY panelWidth signHeight
      piecePics =
        [ drawMiniPiece assets (gySlotColor style) iconSize x (startY - fromIntegral idx * (iconSize + padding)) piece
          | (idx, piece) <- zip [0 :: Int ..] pieces
        ]
   in pictures (panelPic : signPic : piecePics)

drawGraveyardPanel :: GraveyardStyle -> Float -> Float -> Float -> Float -> Picture
drawGraveyardPanel style x y w h =
  let borderSize = 3
   in pictures
        [ translate x y $ color (gyPanelBorder style) $ rectangleSolid (w + borderSize) (h + borderSize),
          translate x y $ color (gyPanelColor style) $ rectangleSolid w h
        ]

drawGraveyardSign :: GraveyardStyle -> Float -> Float -> Float -> Float -> Picture
drawGraveyardSign style x y w h =
  let baseScale = 0.12
      label = gyLabel style
      maxWidth = max 10 (w - 20)
      rawWidth = fromIntegral (length label) * 100 * baseScale
      textScale = min baseScale (maxWidth / rawWidth)
      labelWidth = fromIntegral (length label) * 100 * textScale
      labelX = x - labelWidth / 2
      labelY = y - (h * 0.3)
   in pictures
        [ translate x y $ color (gySignColor style) $ rectangleSolid (w - 12) h,
          translate labelX labelY $
            scale textScale textScale $
              color (gySignTextColor style) $
                text label
        ]

drawMiniPiece :: Assets -> Gloss.Color -> Float -> Float -> Float -> Piece -> Picture
drawMiniPiece assets slotColor iconSize x y piece =
  let (sprite, spriteSize) = pieceSprite assets piece
      scaleFactor = iconSize / spriteSize
   in pictures
        [ translate x y $ color slotColor $ rectangleSolid (iconSize * 1.15) (iconSize * 1.15),
          translate x y $ scale scaleFactor scaleFactor sprite
        ]

drawMessage :: UIState -> Picture
drawMessage state =
  let (width, height) = uiWindowSize state
      x = -width / 2 + 20
      y = -height / 2 + 20
   in translate x y $
        scale 0.15 0.15 $
          color (makeColorI 255 255 255 255) $
            text (fromMaybe (formatStateMessage (uiGameState state)) (uiMessage state))
