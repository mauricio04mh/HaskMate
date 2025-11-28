module Main where

import Board

main :: IO ()
main = do
  putStrLn "HaskMate: Chess Game Initialized"
  let emptyBoard = [] :: Board
  print emptyBoard
