module Main where

import AI.Search.MinimaxPlainStats (minimaxPlainWithStats)
import AI.Search.NegamaxStats (negamaxWithStats)
import AI.SearchStats (SearchStats (..))
import Data.Maybe (isJust)
import GameState (GameState)
import GameState.FEN (parseFEN)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "╔═══════════════════════════════════════════════════════════════╗"
  putStrLn "║         HaskMate - AI Search Algorithm Benchmark            ║"
  putStrLn "╚═══════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- Test positions
  let positions =
        [ ("Starting Position", "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
          ("Middle Game", "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 4 5"),
          ("Complex Position", "r2qkb1r/ppp2ppp/2n1bn2/3pp3/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 4 6")
        ]

  -- Test depths (limited to 4 as per user request)
  let depths = [1, 2, 3, 4]

  putStrLn "Running benchmarks (depths 1-4)..."
  putStrLn "This may take a few moments...\n"

  mapM_ (\(posName, fen) -> benchmarkPosition posName fen depths) positions

  putStrLn "\n╔═══════════════════════════════════════════════════════════════╗"
  putStrLn "║                    Benchmark Complete                       ║"
  putStrLn "╚═══════════════════════════════════════════════════════════════╝"

benchmarkPosition :: String -> String -> [Int] -> IO ()
benchmarkPosition posName fen depths = do
  putStrLn $ "┌─ " ++ posName ++ " " ++ replicate (60 - length posName) '─'
  putStrLn $ "│  FEN: " ++ fen
  putStrLn "│"

  gameState <- case parseFEN fen of
    Left err -> do
      putStrLn $ "ERROR: Invalid FEN: " ++ err
      exitFailure
    Right gs -> pure gs

  putStrLn "│  ┌────────────────────────────────────────────────────────────┐"
  putStrLn "│  │ Depth │ Algorithm      │  Nodes │  Time  │ Pruning Eff. │"
  putStrLn "│  ├────────────────────────────────────────────────────────────┤"

  mapM_ (benchmarkAtDepth gameState) depths

  putStrLn "│  └────────────────────────────────────────────────────────────┘"
  putStrLn "└────────────────────────────────────────────────────────────────\n"

benchmarkAtDepth :: GameState -> Int -> IO ()
benchmarkAtDepth gameState depth = do
  -- Benchmark MinimaxPlain
  maybePlainResult <- timeout (2 * 1000000) $ minimaxPlainWithStats depth gameState

  plainStats <- case maybePlainResult of
    Nothing -> do
      putStrLn $
        printf
          "│  │   %d   │ MinimaxPlain   │    --- │ TIMEOUT │      N/A     │"
          depth
      pure Nothing
    Just (_, stats) -> do
      let plainTime = formatTime (elapsedPicos stats)
      putStrLn $
        printf
          "│  │   %d   │ MinimaxPlain   │ %6d │ %6s │      N/A     │"
          depth
          (nodesVisited stats)
          plainTime
      pure (Just stats)

  -- Benchmark Negamax
  maybeNegaResult <- timeout (2 * 1000000) $ negamaxWithStats depth gameState

  case maybeNegaResult of
    Nothing ->
      putStrLn $
        printf
          "│  │   %d   │ Negamax        │    --- │ TIMEOUT │      ---     │"
          depth
    Just (_, negaStats) -> do
      let negaTime = formatTime (elapsedPicos negaStats)
          pruningEff = case plainStats of
            Just pStats -> calculatePruningEfficiency pStats negaStats
            Nothing -> 0.0 -- Cannot calculate efficiency if plain timed out
          effStr =
            if isJust plainStats
              then printf "%5.1f%%" pruningEff
              else "  N/A  " :: String

      putStrLn $
        printf
          "│  │   %d   │ Negamax        │ %6d │ %6s │    %s    │"
          depth
          (nodesVisited negaStats)
          negaTime
          effStr

formatTime :: Integer -> String
formatTime picos
  | seconds >= 1.0 = printf "%.2fs" seconds
  | millis >= 1.0 = printf "%.1fms" millis
  | otherwise = printf "%.0fµs" micros
  where
    seconds = fromIntegral picos / 1e12 :: Double
    millis = fromIntegral picos / 1e9 :: Double
    micros = fromIntegral picos / 1e6 :: Double

calculatePruningEfficiency :: SearchStats -> SearchStats -> Double
calculatePruningEfficiency plainStats negaStats =
  let plainNodes = fromIntegral (nodesVisited plainStats) :: Double
      negaNodes = fromIntegral (nodesVisited negaStats) :: Double
      reduction = (plainNodes - negaNodes) / plainNodes * 100
   in reduction
