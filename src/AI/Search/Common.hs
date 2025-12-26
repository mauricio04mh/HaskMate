module AI.Search.Common
  ( getAllLegalMoves,
    orderMoves,
    staticScore,
    pickBest,
    pickBestByScore,
    inf,
  )
where

import AI.Evaluation (evaluate)
import Board.Query (boardPieceAt)
import Data.List (foldl', maximumBy, minimumBy, sortBy)
import Data.Maybe (isJust)
import Data.Ord (Down (..), comparing)
import GameState (GameState, applyMove, gsActiveColor, gsBoard, isLegalMove)
import GameState.MoveGen (generateMovesForPiece, piecePositions)
import GameState.Validation (boardIsInCheck)
import Move (Move (..), promotion, toPos)
import Piece (Color (White), getPieceColor)

-- | Get all legal moves for the current position
getAllLegalMoves :: GameState -> [Move]
getAllLegalMoves gameState =
  [ move
    | (from, piece) <- piecePositions (gsBoard gameState) (gsActiveColor gameState),
      move <- generateMovesForPiece gameState from piece,
      isLegalMove gameState move
  ]

-- | Order moves to improve alpha-beta pruning efficiency
-- Prioritizes: captures, promotions, and checks
orderMoves :: GameState -> [Move] -> [Move]
orderMoves gameState moves =
  sortBy (comparing (Down . movePriority)) moves
  where
    movePriority move =
      (isCapture move, isPromotion move, givesCheck move)
    isCapture move =
      case boardPieceAt (gsBoard gameState) (toPos move) of
        Just piece -> getPieceColor piece /= gsActiveColor gameState
        Nothing -> False
    isPromotion move = isJust (promotion move)
    givesCheck move =
      case applyMove gameState move of
        Nothing -> False
        Just nextState ->
          boardIsInCheck (gsBoard nextState) (gsActiveColor nextState)

-- | Static evaluation from the perspective of the active player
staticScore :: GameState -> Int
staticScore gameState =
  case gsActiveColor gameState of
    White -> evaluate gameState
    _ -> negate (evaluate gameState)

-- | Pick the best move based on maximizing or minimizing
pickBest :: Bool -> [(Int, Move)] -> Maybe (Int, Move)
pickBest _ [] = Nothing
pickBest True xs = Just (maximumBy (comparing fst) xs)
pickBest False xs = Just (minimumBy (comparing fst) xs)

-- | Pick the best move by score (maximizing)
pickBestByScore :: [(Int, Move)] -> Maybe (Int, Move)
pickBestByScore [] = Nothing
pickBestByScore (x : xs) = Just (foldl' pick x xs)
  where
    pick best@(bestScore, _) candidate@(score, _)
      | score >= bestScore = candidate
      | otherwise = best

-- | Infinity constant for alpha-beta bounds
inf :: Int
inf = 1000000000
