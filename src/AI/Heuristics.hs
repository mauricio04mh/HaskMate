module AI.Heuristics
  ( kingSafetyScore,
    pawnStructureScore,
  )
where

import Board.Core (Board, CastlingRights (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GameState.MoveGen (piecePositions)
import GameState.Types (toggleColor)
import GameState.Validation (boardIsInCheck, fileIndex, rankIndex)
import Piece (Color (Black, White), PieceType (..), getPieceType)
import Position (File (..), Position (..), Rank (..))

kingSafetyScore :: Board -> CastlingRights -> Int
kingSafetyScore board rights =
  kingScoreFor White - kingScoreFor Black
  where
    kingScoreFor color =
      castledBonusFor color + rightsBonusFor color - checkPenaltyFor color
    castledBonusFor color =
      case kingPosition board color of
        Just pos | isCastled color pos -> castledBonus
        _ -> 0
    rightsBonusFor White =
      castlingRightsBonus * countTrue [whiteKingside rights, whiteQueenside rights]
    rightsBonusFor Black =
      castlingRightsBonus * countTrue [blackKingside rights, blackQueenside rights]
    checkPenaltyFor color =
      if boardIsInCheck board color then checkPenalty else 0

kingPosition :: Board -> Color -> Maybe Position
kingPosition board color =
  case [pos | (pos, piece) <- piecePositions board color, getPieceType piece == King] of
    (pos : _) -> Just pos
    [] -> Nothing

isCastled :: Color -> Position -> Bool
isCastled White (Position (File f) (Rank r)) = r == 1 && f `elem` [3, 7]
isCastled Black (Position (File f) (Rank r)) = r == 8 && f `elem` [3, 7]

pawnStructureScore :: Board -> Int
pawnStructureScore board =
  pawnScoreFor White - pawnScoreFor Black
  where
    pawnScoreFor color =
      let pawnsList = pawns color
          fileMap = pawnFiles pawnsList
          opponentPawns = [p | (p, piece) <- piecePositions board (toggleColor color), getPieceType piece == Pawn]
          passedCount = length (filter (isPassedPawn color opponentPawns) pawnsList)
          doubledCount = sum [count - 1 | count <- Map.elems fileMap, count > 1]
          isolatedCount = length (filter (isIsolated fileMap) pawnsList)
       in passedPawnBonus * passedCount
            - doubledPawnPenalty * doubledCount
            - isolatedPawnPenalty * isolatedCount
    pawns color = [pos | (pos, piece) <- piecePositions board color, getPieceType piece == Pawn]
    pawnFiles pawnsList =
      Map.fromListWith
        (+)
        [ (fileIndex (posFile pos), 1 :: Int)
          | pos <- pawnsList
        ]
    isIsolated fileMap pos =
      let files = Set.fromList (Map.keys fileMap)
          f = fileIndex (posFile pos)
       in not (Set.member (f - 1) files || Set.member (f + 1) files)
    isPassedPawn color opponentPawns pos =
      let f = fileIndex (posFile pos)
          r = rankIndex (posRank pos)
          ahead p =
            let pf = fileIndex (posFile p)
                pr = rankIndex (posRank p)
             in abs (pf - f) <= 1
                  && if color == White then pr > r else pr < r
       in not (any ahead opponentPawns)

countTrue :: [Bool] -> Int
countTrue = length . filter id

castledBonus :: Int
castledBonus = 30

castlingRightsBonus :: Int
castlingRightsBonus = 5

checkPenalty :: Int
checkPenalty = 40

doubledPawnPenalty :: Int
doubledPawnPenalty = 15

isolatedPawnPenalty :: Int
isolatedPawnPenalty = 10

passedPawnBonus :: Int
passedPawnBonus = 20
