module AI.Heuristics.Pawn
  ( pawnStructureScore,
  )
where

import Board.Core (Board)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GameState.MoveGen (piecePositions)
import GameState.Types (toggleColor)
import GameState.Validation (fileIndex, rankIndex)
import Piece (Color (Black, White), PieceType (..), getPieceType)
import Position (Position (..))

pawnStructureScore :: Board -> Int
pawnStructureScore board =
  pawnScore board White - pawnScore board Black

pawnScore :: Board -> Color -> Int
pawnScore board color =
  let pawnsList = getPawns board color
      fileMap = getPawnFiles pawnsList
      opponentPawns = getPawns board (toggleColor color)
      passedCount = length $ filter (isPassedPawn color opponentPawns) pawnsList
      doubledCount = countDoubledPawns fileMap
      isolatedCount = length $ filter (isIsolated fileMap) pawnsList
   in (passedPawnBonus * passedCount)
        - (doubledPawnPenalty * doubledCount)
        - (isolatedPawnPenalty * isolatedCount)

passedPawnBonus, doubledPawnPenalty, isolatedPawnPenalty :: Int
passedPawnBonus = 50
doubledPawnPenalty = 20
isolatedPawnPenalty = 30

countDoubledPawns :: Map.Map Int Int -> Int
countDoubledPawns fileMap = sum [count - 1 | count <- Map.elems fileMap, count > 1]

isPassedPawn :: Color -> [Position] -> Position -> Bool
isPassedPawn color opponentPawns (Position f r) =
  let fIdx = fileIndex f
      rIdx = rankIndex r
      isAhead (Position of' or') =
        let ofIdx = fileIndex of'
            orIdx = rankIndex or'
         in abs (fIdx - ofIdx) <= 1 && if color == White then orIdx > rIdx else orIdx < rIdx
   in not $ any isAhead opponentPawns

getPawns :: Board -> Color -> [Position]
getPawns board color = [pos | (pos, piece) <- piecePositions board color, getPieceType piece == Pawn]

getPawnFiles :: [Position] -> Map.Map Int Int
getPawnFiles pawnsList =
  Map.fromListWith (+) [(fileIndex (posFile pos), 1) | pos <- pawnsList]

isIsolated :: Map.Map Int Int -> Position -> Bool
isIsolated fileMap pos =
  let files = Map.keysSet fileMap
      f = fileIndex (posFile pos)
   in not (Set.member (f - 1) files || Set.member (f + 1) files)
