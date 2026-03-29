module Render (renderGame, loadApplePicture) where

import Graphics.Gloss
import Types

modeLabel :: GameMode -> String
modeLabel Classic = "Classic"
modeLabel Wrap = "Wrap"
modeLabel MultiFood = "MultiFood"

loadApplePicture :: IO Picture
loadApplePicture = loadBMP "data/apple-pixel.bmp"

renderGame :: Picture -> GameState -> Picture
renderGame applePic gs | currentScreen gs == Menu = renderMenu gs
  | otherwise = Pictures
        [ renderBoard gs,
          renderGrid gs,
          renderFood applePic gs,
          renderSnake gs,
          renderScore gs,
          renderBestScore gs,
          renderMode gs,
          renderEsc gs,
          renderPaused gs,
          renderGameOver gs
        ]

renderMenu :: GameState -> Picture
renderMenu gs = Pictures
    [ renderBoard gs,
      translate (-150) 150 $ scale 0.8 0.8 $
          color rose $ Text "SNAKE",
      translate (-125) 90 $ scale 0.25 0.25 $
          color yellow $ Text ("BEST SCORE: " ++ show (bestScore gs)),
      translate (-90) 60 $ scale 0.15 0.15 $
          color orange $ Text ("Current mode: " ++ modeLabel (gameMode gs)),
      translate (-65) 20 $ scale 0.13 0.13 $
          color white $ Text "ENTER - Start",
      translate (-65) (-20) $ scale 0.13 0.13 $
          color white $ Text "ARROWS - Move",
      translate (-65) (-60) $ scale 0.13 0.13 $
          color white $ Text "1 - Classic mode",
      translate (-65) (-100) $ scale 0.13 0.13 $
          color white $ Text "2 - Wrap mode",
      translate (-65) (-140) $ scale 0.13 0.13 $
          color white $ Text "3 - MultiFood mode",
      translate (-65) (-180) $ scale 0.13 0.13 $
          color white $ Text "P - Pause",
      translate (-65) (-220) $ scale 0.13 0.13 $
          color white $ Text "R - Restart",
      translate (-65) (-260) $ scale 0.13 0.13 $
          color white $ Text "Q - Quit"
    ]

renderBoard :: GameState -> Picture
renderBoard gs = color (greyN 0.15) $ rectangleSolid boardPxW boardPxH
  where
    boardPxW = fromIntegral (boardW gs) * cellSize gs
    boardPxH = fromIntegral (boardH gs) * cellSize gs

renderGrid :: GameState -> Picture
renderGrid gs = color (greyN 0.22) $ Pictures (verticals ++ horizontals)
  where
    size = cellSize gs
    w = fromIntegral (boardW gs) * size
    h = fromIntegral (boardH gs) * size
    x0 = -w / 2
    y0 = -h / 2
    verticals = [ Line [(x0 + fromIntegral i * size, y0), (x0 + fromIntegral i * size, y0 + h)]
      | i <- [0 .. boardW gs]]
    horizontals = [ Line [(x0, y0 + fromIntegral i * size), (x0 + w, y0 + fromIntegral i * size)]
      | i <- [0 .. boardH gs]]

renderSnake :: GameState -> Picture
renderSnake gs =
    case snakeBody gs of
      [] -> Blank
      (h:rest) -> Pictures
        [ renderCell gs (dark green) h,
          Pictures (map (renderCell gs (light green)) rest)
        ]

renderFood :: Picture -> GameState -> Picture
renderFood applePic gs = Pictures (map (renderFoodCell gs applePic) (foodCells gs))

renderFoodCell :: GameState -> Picture -> Cell -> Picture
renderFoodCell gs applePic (Cell (x, y)) = translate px py $
    scale scaleFactor scaleFactor $ applePic
  where
    size = cellSize gs
    totalW = fromIntegral (boardW gs) * size
    totalH = fromIntegral (boardH gs) * size
    px = fromIntegral x * size - totalW / 2 + size / 2
    py = fromIntegral y * size - totalH / 2 + size / 2
    scaleFactor = size / 350

renderCell :: GameState -> Color -> Cell -> Picture
renderCell gs c (Cell (x, y)) = translate px py $ color c $
    rectangleSolid (cellSize gs - 2) (cellSize gs - 2)
  where
    size = cellSize gs
    totalW = fromIntegral (boardW gs) * size
    totalH = fromIntegral (boardH gs) * size
    px = fromIntegral x * size - totalW / 2 + size / 2
    py = fromIntegral y * size - totalH / 2 + size / 2

renderScore :: GameState -> Picture
renderScore gs = translate (-290) 320 $ scale 0.15 0.15 $
    color white $ Text ("Score: " ++ show (score gs))

renderMode :: GameState -> Picture
renderMode gs = translate (-50) 320 $ scale 0.15 0.15 $
    color yellow $ Text ("Mode: " ++ modeLabel (gameMode gs))

renderBestScore :: GameState -> Picture
renderBestScore gs = translate 170 320 $ scale 0.15 0.15 $
    color white $ Text ("Best: " ++ show (bestScore gs))

renderEsc :: GameState -> Picture
renderEsc _ = translate (-50) (-320) $ scale 0.12 0.12 $
    color white $ Text "ESC - Menu"

renderPaused :: GameState -> Picture
renderPaused gs | not (isPaused gs) = Blank
  | otherwise = Pictures
        [ color (makeColor 0 0 0 0.45) $
            rectangleSolid 420 120,
          translate (-65) 15 $ scale 0.3 0.3 $
            color white $ Text "PAUSED",
          translate (-85) (-25) $ scale 0.15 0.15 $
            color white $ Text "Press P to continue"
        ]

renderGameOver :: GameState -> Picture
renderGameOver gs | gameStatus gs /= GameOver = Blank
  | otherwise = Pictures
        [ color (makeColor 0 0 0 0.55) $
            rectangleSolid 520 170,
          translate (-125) 20 $ scale 0.30 0.30 $
            color red $ Text "GAME OVER",
          translate (-55) (-20) $ scale 0.2 0.2 $
            color white $ Text ("Score: " ++ show (score gs)),
          translate (-55) (-55) $ scale 0.15 0.15 $
            color white $ Text "R - Restart"
        ]
