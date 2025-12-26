module Main where

import AI.Perft (perft, perftDivide)
import GameState (GameState)
import GameState.FEN (parseFEN)
import Move (Move (..))
import Piece (PieceType (..))
import Position (File (..), Position (..), Rank (..))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

type NamedPosition = (String, String, [Int])

main :: IO ()
main = do
  mapM_ runPosition perftPositions

perftPositions :: [NamedPosition]
perftPositions =
  [ ( "Initial",
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
      [1, 2, 3, 4]
    ),
    ( "Position 3",
      "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1",
      [1, 2, 3, 4]
    ),
    ( "Kiwipete",
      "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
      [1, 2, 3]
    )
  ]

runPosition :: NamedPosition -> IO ()
runPosition (name, fen, depths) = do
  putStrLn ("Position: " ++ name)
  case parseFEN fen of
    Left err -> error ("perft: failed to parse FEN: " ++ err)
    Right gameState -> do
      mapM_ (runDepth gameState) depths
      case reverse depths of
        (d : _) -> runDivide gameState d
        [] -> return ()
  putStrLn ""

runDepth :: GameState -> Int -> IO ()
runDepth gameState depth = do
  start <- getCPUTime
  let nodes = perft depth gameState
  end <- getCPUTime
  let diff = end - start
      elapsedSec = fromIntegral diff / 1e12 :: Double
      nps = if elapsedSec > 0 then fromIntegral nodes / elapsedSec else 0
  printf "  depth %d | nodes %d | time %.6fs | nps %.0f\n" depth nodes elapsedSec nps

runDivide :: GameState -> Int -> IO ()
runDivide gameState depth = do
  putStrLn $ "  Divide at depth " ++ show depth ++ ":"
  let results = perftDivide depth gameState
  mapM_ (\(m, n) -> printf "    %s: %d\n" (formatMove m) n) results
  let total = sum (map snd results)
  printf "    Total: %d\n" total

formatMove :: Move -> String
formatMove (Move (Position (File f1) (Rank r1)) (Position (File f2) (Rank r2)) promo) =
  [fileToChar f1, rankToChar r1, fileToChar f2, rankToChar r2] ++ promoChar promo
  where
    fileToChar f = "abcdefgh" !! (f - 1)
    rankToChar r = "12345678" !! (r - 1)
    promoChar Nothing = ""
    promoChar (Just Queen) = "q"
    promoChar (Just Rook) = "r"
    promoChar (Just Bishop) = "b"
    promoChar (Just Knight) = "n"
    promoChar (Just _) = ""
