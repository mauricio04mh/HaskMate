module Board.Query
  ( boardCellAt,
    boardPieceAt,
    isCellOccupied,
    setPieceAt,
    clearCellAt
  )
where

import Board.Core (Board, Cell, cellPiece)
import Piece (Piece)
import Position (File (..), Position (..), Rank (..))

boardCellAt :: Board -> Position -> Maybe Cell
boardCellAt board pos = do
  (rowIdx, colIdx) <- positionToIndices pos
  row <- safeIndex board rowIdx
  safeIndex row colIdx

boardPieceAt :: Board -> Position -> Maybe Piece
boardPieceAt board pos = do
  cell <- boardCellAt board pos
  cellPiece cell

isCellOccupied :: Board -> Position -> Bool
isCellOccupied board pos =
  case boardPieceAt board pos of
    Just _ -> True
    Nothing -> False

setPieceAt :: Board -> Position -> Maybe Piece -> Board
setPieceAt board pos piece =
  case positionToIndices pos of
    Nothing -> board
    Just (rowIdx, colIdx) ->
      case splitAt rowIdx board of
        (before, targetRow : after) ->
          let updatedRow = updateRow targetRow colIdx piece
           in before ++ (updatedRow : after)
        _ -> board

clearCellAt :: Board -> Position -> Board
clearCellAt board pos = setPieceAt board pos Nothing

positionToIndices :: Position -> Maybe (Int, Int)
positionToIndices (Position (File f) (Rank r))
  | f >= 1 && f <= 8 && r >= 1 && r <= 8 = Just (r - 1, f - 1)
  | otherwise = Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs idx
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

updateRow :: [Cell] -> Int -> Maybe Piece -> [Cell]
updateRow row colIdx newPiece =
  case splitAt colIdx row of
    (before, target : after) ->
      let updated = target {cellPiece = newPiece}
       in before ++ (updated : after)
    _ -> row
