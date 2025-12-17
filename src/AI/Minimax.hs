module AI.Minimax
  ( evaluate,
    minimax,
    getAllLegalMoves,
  )
where

import Data.List (maximumBy, minimumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import Board.Core (Board)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult, isLegalMove)
import GameState.MoveGen (generateMovesForPiece, piecePositions)
import Move (Move)
import Piece (Color (Black, White), Piece (..), PieceType (..), getPieceType)

evaluate :: GameState -> Int
evaluate gameState =
  case gsResult gameState of
    Checkmate winner -> mateScoreFor winner
    DrawBy50Moves -> 0
    DrawByRepetition -> 0
    Stalemate -> 0
    DrawByAgreement -> 0
    Ongoing -> materialScore (gsBoard gameState)
  where
    mateScoreFor White = mateScore
    mateScoreFor Black = negate mateScore

mateScore :: Int
mateScore = 100000

materialScore :: Board -> Int
materialScore board =
  totalValue White board - totalValue Black board

totalValue :: Color -> Board -> Int
totalValue color board =
  sum [pieceValue piece | (_, piece) <- piecePositions board color]

pieceValue :: Piece -> Int
pieceValue piece =
  case getPieceType piece of
    Pawn -> 100
    Knight -> 320
    Bishop -> 330
    Rook -> 500
    Queen -> 900
    King -> 20000

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
        ( \move ->
            fmap (minimaxScore (depth - 1)) (applyMove gameState move)
        )
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
