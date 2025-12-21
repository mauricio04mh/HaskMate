module Main where

import AI.Evaluation (evaluate, mateScore)
import AI.Perft (perft)
import GameState (GameState (..), Result (..), initialGameState)
import GameState.FEN (parseFEN)
import Piece (Color (..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec $ do
  describe "perft" $ do
    perftCases
  describe "evaluate" $ do
    it "initial position evaluates near zero" $
      abs (evaluate initialGameState) `shouldSatisfy` (<= 5)
    it "checkmate for white is mateScore" $
      evaluate (initialGameState {gsResult = Checkmate White}) `shouldBe` mateScore
    it "checkmate for black is -mateScore" $
      evaluate (initialGameState {gsResult = Checkmate Black}) `shouldBe` negate mateScore
    it "draw is zero" $
      evaluate (initialGameState {gsResult = DrawByAgreement}) `shouldBe` 0

perftCases :: Spec
perftCases = do
  let initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      position3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
      kiwipeteFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  describe "initial position" $
    mapM_ (perftShouldBe initialFen)
      [ (1, 20),
        (2, 400),
        (3, 8902),
        (4, 197281)
      ]
  describe "position 3" $
    mapM_ (perftShouldBe position3Fen)
      [ (1, 14),
        (2, 191),
        (3, 2812),
        (4, 43238)
      ]
  describe "kiwipete" $
    mapM_ (perftShouldBe kiwipeteFen)
      [ (1, 48),
        (2, 2039),
        (3, 97862)
      ]

perftShouldBe :: String -> (Int, Integer) -> Spec
perftShouldBe fen (depth, expected) =
  it ("depth " ++ show depth) $
    perftFromFen fen depth `shouldBe` expected

perftFromFen :: String -> Int -> Integer
perftFromFen fen depth =
  case parseFEN fen of
    Left err -> error ("parseFEN failed: " ++ err)
    Right gameState -> perft depth gameState
