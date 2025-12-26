module AI.Search.Quiescence
  ( qsearch,
    qsearchWithStats,
    qsearchTimed,
    tacticalMoves,
  )
where

import AI.Search.Common (getAllLegalMoves, staticScore)
import AI.Search.SearchConfig (SearchLimits (..), StopReason (..))
import AI.Search.TimedState (Deadline, SearchState (..))
import AI.SearchStats (SearchStats (..))
import Board.Query (boardPieceAt)
import Control.Monad.State.Strict (State, get, modify')
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (Down (..), comparing)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult)
import Move (Move (..), promotion, toPos)
import Piece (getPieceColor)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)

-- | Returns only tactical legal moves (captures and promotions).
tacticalMoves :: GameState -> [Move]
tacticalMoves gameState =
  filter isTactical (getAllLegalMoves gameState)
  where
    isTactical move = isCapture move || isPromotion move
    isPromotion move = isJust (promotion move)
    isCapture move =
      case boardPieceAt (gsBoard gameState) (toPos move) of
        Just piece -> getPieceColor piece /= gsActiveColor gameState
        Nothing ->
          -- TODO: detect en passant captures if supported.
          False

orderedTacticalMoves :: GameState -> [Move]
orderedTacticalMoves gameState =
  sortBy (comparing (Down . movePriority)) (tacticalMoves gameState)
  where
    movePriority move =
      ( isPromotion move,
        isCapture move
      )
    isPromotion move = isJust (promotion move)
    isCapture move =
      case boardPieceAt (gsBoard gameState) (toPos move) of
        Just piece -> getPieceColor piece /= gsActiveColor gameState
        Nothing -> False

-- | Negamax alpha-beta quiescence search with depth limit.
qsearch :: Int -> Int -> Int -> GameState -> Int
qsearch qDepthRemaining alpha beta gameState
  | qDepthRemaining <= 0 = staticScore gameState
  | gsResult gameState /= Ongoing = staticScore gameState
  | standPat >= beta = standPat
  | otherwise = search (orderedTacticalMoves gameState) (max alpha standPat)
  where
    standPat = staticScore gameState
    search [] best = best
    search (move : rest) best =
      case applyMove gameState move of
        Nothing -> search rest best
        Just nextState ->
          let score = negate (qsearch (qDepthRemaining - 1) (-beta) (-best) nextState)
              best' = max best score
           in if best' >= beta
                then best'
                else search rest best'

-- | Quiescence search with statistics.
qsearchWithStats :: Int -> Int -> Int -> Int -> GameState -> State SearchStats Int
qsearchWithStats qDepthRemaining alpha beta ply gameState = do
  modify'
    ( \stats ->
        stats
          { nodesVisited = nodesVisited stats + 1,
            maxPlyReached = max (maxPlyReached stats) ply
          }
    )
  let standPat = staticScore gameState
  modify' (\stats -> stats {leafEvals = leafEvals stats + 1})
  if qDepthRemaining <= 0 || gsResult gameState /= Ongoing
    then pure standPat
    else
      if standPat >= beta
        then pure standPat
        else do
          let moves = orderedTacticalMoves gameState
              alpha' = max alpha standPat
          modify' (\stats -> stats {generatedMoves = generatedMoves stats + length moves})
          search moves alpha'
  where
    search [] best = pure best
    search (move : rest) best =
      case applyMove gameState move of
        Nothing -> search rest best
        Just nextState -> do
          score <- fmap negate (qsearchWithStats (qDepthRemaining - 1) (-beta) (-best) (ply + 1) nextState)
          let best' = max best score
          if best' >= beta
            then pure best'
            else search rest best'

-- | Quiescence search with time control and statistics.
qsearchTimed :: SearchLimits -> Deadline -> Int -> Int -> Int -> Int -> GameState -> State SearchState (Either StopReason Int)
qsearchTimed limits deadline qDepthRemaining alpha beta ply gameState = do
  modify'
    ( \st ->
        st
          { ssStats =
              (ssStats st)
                { nodesVisited = nodesVisited (ssStats st) + 1,
                  maxPlyReached = max (maxPlyReached (ssStats st)) ply
                },
            ssNodeCount = ssNodeCount st + 1
          }
    )
  timeResult <- checkTime
  case timeResult of
    Left stop -> pure (Left stop)
    Right () -> do
      let standPat = staticScore gameState
      modify' (\st -> st {ssStats = (ssStats st) {leafEvals = leafEvals (ssStats st) + 1}})
      if qDepthRemaining <= 0 || gsResult gameState /= Ongoing
        then pure (Right standPat)
        else
          if standPat >= beta
            then pure (Right standPat)
            else do
              let moves = orderedTacticalMoves gameState
                  alpha' = max alpha standPat
              modify'
                ( \st ->
                    st
                      { ssStats =
                          (ssStats st)
                            { generatedMoves = generatedMoves (ssStats st) + length moves
                            }
                      }
                )
              search moves alpha'
  where
    checkTime :: State SearchState (Either StopReason ())
    checkTime = do
      st <- get
      let checkEvery = max 1 (slCheckEveryN limits)
      if ssNodeCount st < checkEvery
        then pure (Right ())
        else do
          let now = getTimePicos ()
          if now >= deadline
            then pure (Left TimeUp)
            else do
              modify' (\st' -> st' {ssNodeCount = 0})
              pure (Right ())

    getTimePicos :: () -> Integer
    getTimePicos () = unsafePerformIO getCPUTime
    {-# NOINLINE getTimePicos #-}

    search [] best = pure (Right best)
    search (move : rest) best =
      case applyMove gameState move of
        Nothing -> search rest best
        Just nextState -> do
          result <- qsearchTimed limits deadline (qDepthRemaining - 1) (-beta) (-best) (ply + 1) nextState
          case result of
            Left stop -> pure (Left stop)
            Right score -> do
              let best' = max best (negate score)
              if best' >= beta
                then pure (Right best')
                else search rest best'
