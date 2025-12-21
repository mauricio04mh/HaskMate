module AI.Search
  ( minimax,
    getAllLegalMoves,
  )
where

import AI.Evaluation (evaluate)
import Data.List (maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult, isLegalMove)
import GameState.MoveGen (generateMovesForPiece, piecePositions)
import Move (Move)
import Piece (Color (White))

minimax :: Int -> GameState -> Maybe Move
minimax depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  pure (minimaxScore (depth - 1) nextState, move)
              )
              legalMoves
       in snd <$> pickBest maximizing scoredMoves

minimaxScore :: Int -> GameState -> Int
minimaxScore depth gameState
  | depth <= 0 = evaluate gameState
  | gsResult gameState /= Ongoing = evaluate gameState
  | null legalMoves = evaluate gameState
  | null childScores = evaluate gameState
  | gsActiveColor gameState == White = maximum childScores
  | otherwise = minimum childScores
  where
    legalMoves = getAllLegalMoves gameState
    childScores =
      mapMaybe
        (fmap (minimaxScore (depth - 1)) . applyMove gameState)
        legalMoves

pickBest :: Bool -> [(Int, Move)] -> Maybe (Int, Move)
pickBest _ [] = Nothing
pickBest True xs = Just (maximumBy (comparing fst) xs)
pickBest False xs = Just (minimumBy (comparing fst) xs)

getAllLegalMoves :: GameState -> [Move]
getAllLegalMoves gameState =
  [ move
    | (from, piece) <- piecePositions (gsBoard gameState) (gsActiveColor gameState),
      move <- generateMovesForPiece gameState from piece,
      isLegalMove gameState move
  ]
