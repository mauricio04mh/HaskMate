module Main where

import Graphics.Gloss (Color, InWindow, makeColorI)
import Graphics.Gloss.Interface.Pure.Game (play)

import UI.Controller (handleEvent, stepSimulation)
import UI.Renderer (drawUI)
import UI.Types (initialUIState)

window :: InWindow
window = InWindow "HaskMate" (640, 680) (100, 100)

background :: Color
background = makeColorI 30 30 30 255

fps :: Int
fps = 60

main :: IO ()
main =
  play
    window
    background
    fps
    initialUIState
    drawUI
    handleEvent
    stepSimulation
