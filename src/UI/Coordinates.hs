module UI.Coordinates
  ( positionToPoint,
    screenToPosition,
    squareSize,
  )
where

import Position (File (..), Position (..), Rank (..), mkPosition)

squareSize :: Float
squareSize = 80

boardSize :: Float
boardSize = squareSize * 8

halfBoard :: Float
halfBoard = boardSize / 2

positionToPoint :: Position -> (Float, Float)
positionToPoint (Position (File f) (Rank r)) =
  ( (fromIntegral f - 4.5) * squareSize,
    (fromIntegral r - 4.5) * squareSize
  )

screenToPosition :: (Float, Float) -> Maybe Position
screenToPosition (x, y) =
  let normalizedX = x + halfBoard
      normalizedY = y + halfBoard
      file = floor (normalizedX / squareSize) + 1
      rank = floor (normalizedY / squareSize) + 1
   in mkPosition file rank
