module Main where

import Board

main :: IO ()
main = do
  putStrLn "HaskMate: Chess Game Initialized"
  print emptyBoard
