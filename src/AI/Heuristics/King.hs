module AI.Heuristics.King
  ( kingSafetyScore,
  )
where

import Board.Core (Board, CastlingRights (..))
import GameState.MoveGen (piecePositions)
import GameState.Validation (boardIsInCheck)
import Piece (Color (Black, White), PieceType (..), getPieceType)
import Position (File (..), Position (..), Rank (..))

kingSafetyScore :: Board -> CastlingRights -> Int
kingSafetyScore board rights =
  kingScoreFor board rights White - kingScoreFor board rights Black

kingScoreFor :: Board -> CastlingRights -> Color -> Int
kingScoreFor board rights color =
  castledBonusFor board color + rightsBonusFor rights color - checkPenaltyFor board color

castledBonusFor :: Board -> Color -> Int
castledBonusFor board color =
  case kingPosition board color of
    Just pos | isCastled color pos -> castledBonus
    _ -> 0

rightsBonusFor :: CastlingRights -> Color -> Int
rightsBonusFor rights White =
  castlingRightsBonus * countTrue [whiteKingside rights, whiteQueenside rights]
rightsBonusFor rights Black =
  castlingRightsBonus * countTrue [blackKingside rights, blackQueenside rights]

checkPenaltyFor :: Board -> Color -> Int
checkPenaltyFor board color =
  if boardIsInCheck board color then checkPenalty else 0

countTrue :: [Bool] -> Int
countTrue = length . filter id

castledBonus, castlingRightsBonus, checkPenalty :: Int
castledBonus = 50
castlingRightsBonus = 10
checkPenalty = 30

kingPosition :: Board -> Color -> Maybe Position
kingPosition board color =
  case [pos | (pos, piece) <- piecePositions board color, getPieceType piece == King] of
    (pos : _) -> Just pos
    [] -> Nothing

isCastled :: Color -> Position -> Bool
isCastled White (Position (File f) (Rank r)) = r == 1 && f `elem` [3, 7]
isCastled Black (Position (File f) (Rank r)) = r == 8 && f `elem` [3, 7]
