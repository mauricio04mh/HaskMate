module AI.Search
  ( minimax,
    minimaxWithStats,
    getAllLegalMoves,
    staticScore,
    negamaxAB,
    bestMoveAB,
    orderMoves,
  )
where

import AI.Evaluation (evaluate)
import AI.SearchStats (SearchStats (..), emptySearchStats, withElapsedPicos)
import Control.Monad.State.Strict (State, modify', runState)
import Data.List (foldl', maximumBy, minimumBy, sortBy)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Ord (Down (..), comparing)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult, isLegalMove)
import GameState.MoveGen (generateMovesForPiece, piecePositions)
import GameState.Validation (boardIsInCheck)
import Move (Move (..))
import Piece (Color (White), getPieceColor)
import System.CPUTime (getCPUTime)
import Board.Query (boardPieceAt)

minimax :: Int -> GameState -> Maybe Move
minimax depth gameState = bestMoveAB depth gameState

minimaxPlain :: Int -> GameState -> Maybe Move
minimaxPlain depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = getAllLegalMoves gameState
          maximizing = gsActiveColor gameState == White
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  pure (minimaxPlainScore (depth - 1) nextState, move)
              )
              legalMoves
       in snd <$> pickBest maximizing scoredMoves

minimaxWithStats :: Int -> GameState -> IO (Maybe Move, SearchStats)
minimaxWithStats depth gameState
  | depth <= 0 = pure (Nothing, emptySearchStats)
  | otherwise = do
      start <- getCPUTime
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
          action =
            fmap catMaybes $
              mapM
                ( \move ->
                    case applyMove gameState move of
                      Nothing ->
                        error "minimaxWithStats: applyMove failed for a move reported as legal"
                      Just nextState -> do
                        score <- negamaxABWithStats (depth - 1) (-inf) inf 1 nextState
                        pure (Just (negate score, move))
                )
                legalMoves
          (scoredMoves, stats) = runState action emptySearchStats
          bestMove = snd <$> pickBestByScore scoredMoves
      end <- getCPUTime
      let diffPicos = end - start
      pure (bestMove, withElapsedPicos diffPicos stats)

minimaxPlainScore :: Int -> GameState -> Int
minimaxPlainScore depth gameState
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
        (fmap (minimaxPlainScore (depth - 1)) . applyMove gameState)
        legalMoves

minimaxPlainScoreWithStats :: Int -> Int -> GameState -> State SearchStats Int
minimaxPlainScoreWithStats depth ply gameState = do
  modify'
    ( \stats ->
        stats
          { nodesVisited = nodesVisited stats + 1,
            maxPlyReached = max (maxPlyReached stats) ply
          }
    )
  if depth <= 0 || gsResult gameState /= Ongoing
    then leafEval
    else do
      let legalMoves = getAllLegalMoves gameState
      modify' (\stats -> stats {generatedMoves = generatedMoves stats + length legalMoves})
      if null legalMoves
        then leafEval
        else do
          childScores <-
            mapM
              ( \move ->
                  case applyMove gameState move of
                    Nothing ->
                      error "minimaxWithStats: applyMove failed for a move reported as legal"
                    Just nextState -> minimaxPlainScoreWithStats (depth - 1) (ply + 1) nextState
              )
              legalMoves
          if null childScores
            then leafEval
            else
              if gsActiveColor gameState == White
                then pure (maximum childScores)
                else pure (minimum childScores)
  where
    leafEval = do
      modify' (\stats -> stats {leafEvals = leafEvals stats + 1})
      pure (evaluate gameState)

staticScore :: GameState -> Int
staticScore gameState =
  case gsActiveColor gameState of
    White -> evaluate gameState
    _ -> negate (evaluate gameState)

inf :: Int
inf = 1000000000

negamaxAB :: Int -> Int -> Int -> GameState -> Int
negamaxAB depth alpha beta gameState
  | depth <= 0 = staticScore gameState
  | gsResult gameState /= Ongoing = staticScore gameState
  | null legalMoves = staticScore gameState
  | otherwise = go orderedMoves alpha False
  where
    legalMoves = getAllLegalMoves gameState
    orderedMoves = orderMoves gameState legalMoves
    go [] best hadChild =
      if hadChild
        then best
        else staticScore gameState
    go (move : rest) best hadChild =
      case applyMove gameState move of
        Nothing -> go rest best hadChild
        Just nextState ->
          let score = negate (negamaxAB (depth - 1) (-beta) (-best) nextState)
              best' = max best score
           in if best' >= beta
                then best'
                else go rest best' True

negamaxABWithStats :: Int -> Int -> Int -> Int -> GameState -> State SearchStats Int
negamaxABWithStats depth alpha beta ply gameState = do
  modify'
    ( \stats ->
        stats
          { nodesVisited = nodesVisited stats + 1,
            maxPlyReached = max (maxPlyReached stats) ply
          }
    )
  if depth <= 0 || gsResult gameState /= Ongoing
    then leafEval
    else do
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
      modify' (\stats -> stats {generatedMoves = generatedMoves stats + length legalMoves})
      if null legalMoves
        then leafEval
        else go legalMoves alpha False
  where
    leafEval = do
      modify' (\stats -> stats {leafEvals = leafEvals stats + 1})
      pure (staticScore gameState)
    go [] best hadChild =
      if hadChild
        then pure best
        else leafEval
    go (move : rest) best hadChild =
      case applyMove gameState move of
        Nothing -> go rest best hadChild
        Just nextState -> do
          score <- fmap negate (negamaxABWithStats (depth - 1) (-beta) (-best) (ply + 1) nextState)
          let best' = max best score
          if best' >= beta
            then pure best'
            else go rest best' True

bestMoveAB :: Int -> GameState -> Maybe Move
bestMoveAB depth gameState
  | depth <= 0 = Nothing
  | otherwise =
      let legalMoves = orderMoves gameState (getAllLegalMoves gameState)
          scoredMoves =
            mapMaybe
              ( \move -> do
                  nextState <- applyMove gameState move
                  let score = negate (negamaxAB (depth - 1) (-inf) inf nextState)
                  pure (score, move)
              )
              legalMoves
       in snd <$> pickBestByScore scoredMoves

pickBestByScore :: [(Int, Move)] -> Maybe (Int, Move)
pickBestByScore [] = Nothing
pickBestByScore (x : xs) = Just (foldl' pick x xs)
  where
    pick best@(bestScore, _) candidate@(score, _)
      | score >= bestScore = candidate
      | otherwise = best

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
