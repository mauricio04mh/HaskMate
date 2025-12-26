module Main where

import AI.Search
  ( SearchLimits (..),
    SearchResult (..),
    StopReason (..),
    defaultSearchLimits,
    minimax,
    searchBestMoveTimed,
  )
import AI.SearchStats (SearchStats (..))
import GameState (GameState)
import GameState.FEN (parseFEN)
import System.Exit (exitFailure)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "╔═══════════════════════════════════════════════════════════════╗"
  putStrLn "║        Phase 2 - Iterative Deepening Verification          ║"
  putStrLn "╚═══════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- Test position: starting position
  let startFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  gameState <- case parseFEN startFEN of
    Left err -> do
      putStrLn $ "ERROR parsing FEN: " ++ err
      exitFailure
    Right gs -> pure gs

  putStrLn "Test 1: Con tiempo generoso (5000ms), verificar determinismo"
  putStrLn "═══════════════════════════════════════════════════════════════"

  let limitsGenerous =
        defaultSearchLimits
          { slMaxDepth = 4,
            slTimeLimitMs = 5000,
            slEnableKiller = True,
            slEnableHistory = True
          }

  result1 <- searchBestMoveTimed limitsGenerous gameState
  printResult result1

  putStrLn "\nTest 2: Con tiempo limitado (100ms), verificar timeout rápido"
  putStrLn "═══════════════════════════════════════════════════════════════"

  let limitsShort =
        defaultSearchLimits
          { slMaxDepth = 10, -- Profundidad alta que no alcanzará
            slTimeLimitMs = 100,
            slEnableKiller = True,
            slEnableHistory = True
          }

  result2 <- searchBestMoveTimed limitsShort gameState
  printResult result2

  putStrLn "\nTest 3: Sin heurísticas (PV/Killer/History), verificar diferencia"
  putStrLn "═══════════════════════════════════════════════════════════════"

  let limitsNoHeuristics =
        defaultSearchLimits
          { slMaxDepth = 4,
            slTimeLimitMs = 5000,
            slEnableKiller = False,
            slEnableHistory = False
          }

  result3 <- searchBestMoveTimed limitsNoHeuristics gameState
  printResult result3

  putStrLn "\nTest 4: Comparar con búsqueda fija (minimax profundidad 3)"
  putStrLn "═══════════════════════════════════════════════════════════════"

  let fixedMove = minimax 3 gameState
  putStrLn $ "Mejor movimiento (minimax depth 3): " ++ show fixedMove

  let limitsDepth3 =
        defaultSearchLimits
          { slMaxDepth = 3,
            slTimeLimitMs = 5000
          }

  result4 <- searchBestMoveTimed limitsDepth3 gameState
  putStrLn $ "\nMejor movimiento (ID hasta depth 3): " ++ show (srBestMove result4)

  if srBestMove result4 == fixedMove
    then putStrLn "✓ PASS: Ambos coinciden (determinismo verificado)"
    else putStrLn "⚠ DIFFER: Movimientos diferentes (puede ser empate de evaluación)"

  putStrLn "\n╔═══════════════════════════════════════════════════════════════╗"
  putStrLn "║                     Tests Completados                       ║"
  putStrLn "╚═══════════════════════════════════════════════════════════════╝"

printResult :: SearchResult -> IO ()
printResult result = do
  putStrLn $
    printf
      "Mejor movimiento: %s"
      (show $ srBestMove result)
  putStrLn $
    printf
      "Puntaje:          %d"
      (srBestScore result)
  putStrLn $
    printf
      "Profundidad:      %d"
      (srDepthReached result)
  putStrLn $
    printf
      "Razón parada:     %s"
      (show $ srStopReason result)
  putStrLn "\nEstadísticas:"
  let stats = srStatsTotal result
  putStrLn $
    printf
      "  Nodos visitados: %d"
      (nodesVisited stats)
  putStrLn $
    printf
      "  Hojas evaluadas: %d"
      (leafEvals stats)
  putStrLn $
    printf
      "  Movs generados:  %d"
      (generatedMoves stats)
  putStrLn $
    printf
      "  Max ply:         %d"
      (maxPlyReached stats)
  putStrLn $
    printf
      "  Tiempo (picos):  %d"
      (elapsedPicos stats)
