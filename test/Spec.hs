module Main where

import AI.Evaluation (evaluate, mateScore)
import AI.Perft (perft)
import Data.Maybe (fromJust)
import GameState (GameState (..), Result (..), applyMove, initialGameState)
import GameState.FEN (parseFEN)
import Move (Move (..))
import Piece (Color (..))
import Position (File (..), Position (..), Rank (..))
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
    it "insufficient material is zero" $
      evaluate (initialGameState {gsResult = DrawByInsufficientMaterial}) `shouldBe` 0
  describe "repetition draw" $ do
    it "detects threefold repetition (Nf3 Nf6 Ng1 Ng8 x2)" $ do
      let Right start = parseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          m f1 r1 f2 r2 gs = fromJust $ applyMove gs (Move (Position (File f1) (Rank r1)) (Position (File f2) (Rank r2)) Nothing)
          -- 1. Nf3 Nf6
          p1 = m 7 1 6 3 start
          p2 = m 7 8 6 6 p1
          -- 2. Ng1 Ng8
          p3 = m 6 3 7 1 p2
          p4 = m 6 6 7 8 p3
          -- 3. Nf3 Nf6
          p5 = m 7 1 6 3 p4
          p6 = m 7 8 6 6 p5
          -- 4. Ng1 Ng8
          p7 = m 6 3 7 1 p6
          p8 = m 6 6 7 8 p7
      gsResult p8 `shouldBe` DrawByRepetition
  describe "insufficient material draw" $ do
    it "detects king vs king" $ do
      let Right start = parseFEN "8/8/8/8/8/8/8/K6k w - - 0 1"
          move = Move (Position (File 1) (Rank 1)) (Position (File 1) (Rank 2)) Nothing
          nextState = fromJust (applyMove start move)
      gsResult nextState `shouldBe` DrawByInsufficientMaterial
    it "detects same-color bishops" $ do
      let Right start = parseFEN "5b1k/8/8/8/8/8/8/2B1K3 w - - 0 1"
          move = Move (Position (File 5) (Rank 1)) (Position (File 5) (Rank 2)) Nothing
          nextState = fromJust (applyMove start move)
      gsResult nextState `shouldBe` DrawByInsufficientMaterial

perftCases :: Spec
perftCases = do
  let initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      position3Fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"
      kiwipeteFen = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
  describe "initial position" $
    mapM_
      (perftShouldBe initialFen)
      [ (1, 20),
        (2, 400),
        (3, 8902),
        (4, 197281)
      ]
  describe "position 3" $
    mapM_
      (perftShouldBe position3Fen)
      [ (1, 14),
        (2, 191),
        (3, 2812),
        (4, 43238)
      ]
  describe "kiwipete" $
    mapM_
      (perftShouldBe kiwipeteFen)
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
