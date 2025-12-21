module Main where

import Graphics.Gloss (Color, Display (FullScreen), makeColorI)
import Graphics.Gloss.Interface.IO.Game (playIO)
import UI.Assets (loadAssets)
import UI.Controller (handleEvent, stepSimulation)
import UI.Renderer (drawUI)
import UI.Types (initialUIState)

window :: Display
window = FullScreen

background :: Color
background = makeColorI 30 30 30 255

fps :: Int
fps = 60

main :: IO ()
main = do
  assets <- loadAssets
  playIO
    window
    background
    fps
    (initialUIState assets)
    (pure . drawUI)
    handleEvent
    stepSimulation
