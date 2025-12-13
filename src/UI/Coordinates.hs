module UI.Coordinates
  ( positionToPoint,
    screenToPosition,
  )
where

import Position (File (..), Position (..), Rank (..), mkPosition)

positionToPoint :: Float -> Position -> (Float, Float)
positionToPoint squareSize (Position (File f) (Rank r)) =
  ( (fromIntegral f - 4.5) * squareSize,
    (fromIntegral r - 4.5) * squareSize
  )

screenToPosition :: Float -> (Float, Float) -> Maybe Position
screenToPosition squareSize (x, y) =
  let boardSize = squareSize * 8
      halfBoard = boardSize / 2
      normalizedX = x + halfBoard
      normalizedY = y + halfBoard
      file = floor (normalizedX / squareSize) + 1
      rank = floor (normalizedY / squareSize) + 1
   in mkPosition file rank
