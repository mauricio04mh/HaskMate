module AI.Search.Timed
  ( searchBestMoveTimed,
    minimaxTimed,
  )
where

import AI.Search.Common (getAllLegalMoves, inf, staticScore)
import AI.Search.Ordering
  ( emptyOrderContext,
    orderMovesCtx,
    updateHistory,
    updateKiller,
  )
import AI.Search.Quiescence (qsearchTimed)
import AI.Search.SearchConfig
  ( SearchLimits (..),
    SearchResult (..),
    StopReason (..),
  )
import AI.Search.TimedState (Deadline, SearchState (..))
import AI.SearchStats (SearchStats (..), emptySearchStats)
import Board.Query (boardPieceAt)
import Control.Monad.State.Strict (State, get, modify', runState)
import Data.Maybe (isJust)
import GameState (GameState, Result (..), applyMove, gsActiveColor, gsBoard, gsResult)
import Move (Move (..))
import Piece (getPieceColor)
import System.CPUTime (getCPUTime)

qMaxDepth :: Int
qMaxDepth = 8

-- | Negamax with alpha-beta pruning, time control, and context updates
negamaxABTimed ::
  SearchLimits ->
  Deadline ->
  Int -> -- ply
  Int -> -- depthRemaining
  Int -> -- alpha
  Int -> -- beta
  GameState ->
  State SearchState (Either StopReason Int)
negamaxABTimed limits deadline ply depthRemaining alpha beta gameState = do
  -- Update stats
  modify' $ \st ->
    st
      { ssStats =
          (ssStats st)
            { nodesVisited = nodesVisited (ssStats st) + 1,
              maxPlyReached = max (maxPlyReached (ssStats st)) ply
            },
        ssNodeCount = ssNodeCount st + 1
      }

  -- Base cases
  if gsResult gameState /= Ongoing
    then do
      modify' $ \st ->
        st
          { ssStats = (ssStats st) {leafEvals = leafEvals (ssStats st) + 1}
          }
      pure $ Right $ staticScore gameState
    else
      if depthRemaining <= 0
        then qsearchTimed limits deadline qMaxDepth alpha beta ply gameState
        else do
          let legalMoves = getAllLegalMoves gameState

          modify' $ \st ->
            st
              { ssStats =
                  (ssStats st)
                    { generatedMoves = generatedMoves (ssStats st) + length legalMoves
                    }
              }

          if null legalMoves
            then do
              modify' $ \st ->
                st
                  { ssStats = (ssStats st) {leafEvals = leafEvals (ssStats st) + 1}
                  }
              pure $ Right $ staticScore gameState
            else do
              -- Order moves with context
              ctx <- fmap ssContext get
              let orderedMoves = orderMovesCtx ctx gameState ply legalMoves
              searchMoves orderedMoves alpha False
  where
    searchMoves [] best hadChild =
      if hadChild
        then pure (Right best)
        else do
          modify' $ \st ->
            st
              { ssStats = (ssStats st) {leafEvals = leafEvals (ssStats st) + 1}
              }
          pure $ Right $ staticScore gameState
    searchMoves (move : rest) best hadChild = do
      case applyMove gameState move of
        Nothing -> searchMoves rest best hadChild
        Just nextState -> do
          result <- negamaxABTimed limits deadline (ply + 1) (depthRemaining - 1) (-beta) (-best) nextState
          case result of
            Left signal -> pure (Left signal)
            Right score -> do
              let scoreNeg = negate score
                  best' = max best scoreNeg

              -- Update killers and history on beta cutoff for quiet moves
              when (best' >= beta) $ do
                when (slEnableKiller limits || slEnableHistory limits) $ do
                  let isQuiet = not (isCapture move) && not (isPromotion move)
                  when isQuiet $ do
                    ctx <- fmap ssContext get
                    let ctx' =
                          if slEnableKiller limits
                            then updateKiller ply move ctx
                            else ctx
                        ctx'' =
                          if slEnableHistory limits
                            then updateHistory move depthRemaining ctx'
                            else ctx'
                    modify' $ \st -> st {ssContext = ctx''}

              if best' >= beta
                then pure (Right best')
                else searchMoves rest best' True

    isCapture move =
      case boardPieceAt (gsBoard gameState) (toPos move) of
        Just piece -> getPieceColor piece /= gsActiveColor gameState
        Nothing -> False

    isPromotion move = isJust (promotion move)

    when :: Bool -> State SearchState () -> State SearchState ()
    when True action = action
    when False _ = pure ()

-- | Root search for a single depth
rootSearchTimed ::
  SearchLimits ->
  Deadline ->
  Int -> -- depth
  GameState ->
  IO (Either StopReason (Maybe Move, Int, SearchState))
rootSearchTimed limits deadline depth gameState = do
  let legalMoves = getAllLegalMoves gameState
  if null legalMoves
    then do
      let finalState =
            SearchState
              { ssStats = emptySearchStats,
                ssContext = emptyOrderContext,
                ssNodeCount = 0
              }
      pure $ Right (Nothing, staticScore gameState, finalState)
    else do
      -- Get initial context
      let initState =
            SearchState
              { ssStats = emptySearchStats,
                ssContext = emptyOrderContext,
                ssNodeCount = 0
              }

      -- Order moves with context
      let ctx = ssContext initState
          orderedMoves = orderMovesCtx ctx gameState 0 legalMoves

      currentTime <- getCPUTime
      if currentTime >= deadline
        then pure $ Left TimeUp
        else searchRoot orderedMoves Nothing (negate inf) initState
  where
    searchRoot [] bestMove bestScore st =
      pure $ Right (bestMove, bestScore, st)
    searchRoot (move : rest) currentBest currentBestScore st = do
      case applyMove gameState move of
        Nothing -> searchRoot rest currentBest currentBestScore st
        Just nextState -> do
          currentTime <- getCPUTime
          if currentTime >= deadline
            then pure $ Left TimeUp
            else do
              let (result, st') = runState (negamaxABTimed limits deadline 1 (depth - 1) (negate inf) (negate currentBestScore) nextState) st
              case result of
                Left _ ->
                  -- TimeUp or any error: do not accept partial root results
                  pure $ Left TimeUp
                Right score -> do
                  let scoreNeg = negate score
                  if scoreNeg > currentBestScore
                    then searchRoot rest (Just move) scoreNeg st'
                    else searchRoot rest currentBest currentBestScore st'

-- | Main entry point: Iterative Deepening with time control
searchBestMoveTimed :: SearchLimits -> GameState -> IO SearchResult
searchBestMoveTimed limits gameState
  | slMaxDepth limits <= 0 =
      pure $
        SearchResult
          { srBestMove = Nothing,
            srBestScore = 0,
            srDepthReached = 0,
            srStopReason = CompletedDepth,
            srStatsTotal = emptySearchStats
          }
  | otherwise = do
      startTime <- getCPUTime
      let deadlinePicos = startTime + fromIntegral (slTimeLimitMs limits) * 1000000000
      iterativeDeepening deadlinePicos 1 Nothing 0 emptySearchStats emptyOrderContext
  where
    iterativeDeepening deadline depth lastBestMove lastBestScore lastStats _
      | depth > slMaxDepth limits =
          pure $
            SearchResult
              { srBestMove = lastBestMove,
                srBestScore = lastBestScore,
                srDepthReached = depth - 1,
                srStopReason = CompletedDepth,
                srStatsTotal = lastStats
              }
      | otherwise = do
          currentTime <- getCPUTime
          if currentTime >= deadline
            then
              pure $
                SearchResult
                  { srBestMove = lastBestMove,
                    srBestScore = lastBestScore,
                    srDepthReached = depth - 1,
                    srStopReason = TimeUp,
                    srStatsTotal = lastStats
                  }
            else do
              -- We pass the context with PV move set directly to rootSearchTimed
              -- Note: rootSearchTimed currently initializes its own context
              -- TODO: refactor to accept OrderContext parameter and use lastCtx with PV move
              result <- rootSearchTimed limits deadline depth gameState

              case result of
                Left _ ->
                  -- TimeUp or any error
                  pure $
                    SearchResult
                      { srBestMove = lastBestMove,
                        srBestScore = lastBestScore,
                        srDepthReached = depth - 1,
                        srStopReason = TimeUp,
                        srStatsTotal = lastStats
                      }
                Right (Just move, score, st) -> do
                  -- Accumulate stats
                  let newStats = accumulateStats lastStats (ssStats st)
                  iterativeDeepening deadline (depth + 1) (Just move) score newStats (ssContext st)
                Right (Nothing, score, st) ->
                  -- No legal moves (should not happen if we checked earlier)
                  pure $
                    SearchResult
                      { srBestMove = Nothing,
                        srBestScore = score,
                        srDepthReached = depth,
                        srStopReason = CompletedDepth,
                        srStatsTotal = ssStats st
                      }

    -- Accumulate statistics from iterations
    accumulateStats :: SearchStats -> SearchStats -> SearchStats
    accumulateStats old new =
      SearchStats
        { nodesVisited = nodesVisited old + nodesVisited new,
          leafEvals = leafEvals old + leafEvals new,
          generatedMoves = generatedMoves old + generatedMoves new,
          maxPlyReached = max (maxPlyReached old) (maxPlyReached new),
          elapsedPicos = elapsedPicos new -- Keep the most recent elapsed time
        }

-- | Convenience wrapper that just returns the best move
minimaxTimed :: SearchLimits -> GameState -> IO (Maybe Move)
minimaxTimed limits gameState = do
  result <- searchBestMoveTimed limits gameState
  pure $ srBestMove result
