module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Random (newStdGen)
import Logic
import Render
import Input
import Types

main :: IO ()
main = do
  gen <- newStdGen
  best <- loadBestScore
  applePic <- loadApplePicture
  let gs0 = initialState gen
      gs  = gs0 { bestScore = best }
      widthPx  = round (fromIntegral (boardW gs) * cellSize gs)
      heightPx = round (fromIntegral (boardH gs) * cellSize gs) + 80
  playIO
    (InWindow "Snake" (widthPx, heightPx) (100, 100))
    black
    60
    gs
    (\st -> pure (renderGame applePic st))
    handleInput
    stepGameIO
