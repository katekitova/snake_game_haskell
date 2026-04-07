module Render (renderGame, loadApplePicture, loadSnakePictures) where

import Graphics.Gloss
import Types
data SnakePictures = SnakePictures
  { headPic :: Picture,
    mouthOpenHeadPic :: Picture,
    deadHeadPic :: Picture,
    bodyPic :: Picture,
    tailPic :: Picture
  }

modeLabel :: GameMode -> String
modeLabel Classic = "Classic"
modeLabel Wrap = "Wrap"
modeLabel MultiFood = "MultiFood"

loadApplePicture :: IO Picture
loadApplePicture = loadBMP "data/apple-pixel.bmp"

loadSnakePictures :: IO SnakePictures
loadSnakePictures = do
  h <- loadBMP "data/snake_head.bmp"
  hm <- loadBMP "data/snake_head_mouth_open.bmp"
  dh <- loadBMP "data/snake_head_dead.bmp"
  b <- loadBMP "data/snake_body.bmp"
  t <- loadBMP "data/snake_tail.bmp"
  pure SnakePictures
    { headPic = h,
      mouthOpenHeadPic = hm,
      deadHeadPic = dh,
      bodyPic = b,
      tailPic = t
    }

renderGame :: Picture -> SnakePictures -> GameState -> Picture
renderGame applePic snakePics gs | currentScreen gs == Menu = renderMenu gs
  | otherwise = Pictures
        [ renderBoard gs,
          renderGrid gs,
          renderFood applePic gs,
          renderSnake snakePics gs,
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

renderSnake :: SnakePictures -> GameState -> Picture
renderSnake pics gs =
  case snakeBody gs of
    [] -> Blank
    [h] -> renderHead pics gs h
    [h, t] -> Pictures [ renderHead pics gs h, renderTail pics gs [h, t] t]
    cells ->
      let h = head cells
          t = last cells
          middleTriples = zip3 cells (tail cells) (drop 2 cells)
      in Pictures
          [ renderHead pics gs h,
            Pictures (map (renderBodySegment pics gs) middleTriples),
            renderTail pics gs cells t]

renderHead :: SnakePictures -> GameState -> Cell -> Picture
renderHead pics gs cell = renderHeadCell gs sprite angle cell
  where
    sprite | gameStatus gs == GameOver = deadHeadPic pics
      | eatAnimTimer gs > 0 = mouthOpenHeadPic pics
      | otherwise = headPic pics
    angle = directionAngle (currentDir gs)

renderHeadCell :: GameState -> Picture -> Float -> Cell -> Picture
renderHeadCell gs sprite angle (Cell (x, y)) =
  translate px py $ rotate angle $ scale scaleFactor scaleFactor sprite
  where
    size = cellSize gs
    totalW = fromIntegral (boardW gs) * size
    totalH = fromIntegral (boardH gs) * size
    px = fromIntegral x * size - totalW / 2 + size / 2
    py = fromIntegral y * size - totalH / 2 + size / 2
    scaleFactor = size / 800

renderBodySegment :: SnakePictures -> GameState -> (Cell, Cell, Cell) -> Picture
renderBodySegment pics gs (prevCell, curCell, nextCell) =
  renderBodyCell gs (bodyPic pics) angle curCell
  where
    angle = bodyAngle prevCell nextCell

bodyAngle :: Cell -> Cell -> Float
bodyAngle (Cell (px, py)) (Cell (nx, ny)) | px == nx  = 0 | py == ny  = 90 | otherwise = 0  

renderBodyCell :: GameState -> Picture -> Float -> Cell -> Picture
renderBodyCell gs sprite angle (Cell (x, y)) =
  translate px py $
    rotate angle $
      scale scaleFactor scaleFactor sprite
  where
    size = cellSize gs
    totalW = fromIntegral (boardW gs) * size
    totalH = fromIntegral (boardH gs) * size
    px = fromIntegral x * size - totalW / 2 + size / 2
    py = fromIntegral y * size - totalH / 2 + size / 2
    scaleFactor = size / 800

renderTail :: SnakePictures -> GameState -> [Cell] -> Cell -> Picture
renderTail pics gs cells tailCell = renderTailCell gs (tailPic pics) angle tailCell
  where
    angle = tailAngle (tailDirection cells)

renderTailCell :: GameState -> Picture -> Float -> Cell -> Picture
renderTailCell gs sprite angle (Cell (x, y)) =
  translate px py $ rotate angle $ scale scaleFactor scaleFactor sprite
  where
    size = cellSize gs
    totalW = fromIntegral (boardW gs) * size
    totalH = fromIntegral (boardH gs) * size
    px = fromIntegral x * size - totalW / 2 + size / 2
    py = fromIntegral y * size - totalH / 2 + size / 2
    scaleFactor = size / 95

directionAngle :: Direction -> Float
directionAngle U = 0
directionAngle R = 90
directionAngle D = 180
directionAngle L = -90

tailAngle :: Direction -> Float
tailAngle U = 0
tailAngle R = 90
tailAngle D = 180
tailAngle L = -90

tailDirection :: [Cell] -> Direction
tailDirection cells =
  case reverse cells of
    (Cell (tx, ty)) : (Cell (px, py)) : _
      | tx == px && ty > py -> U
      | tx == px && ty < py -> D
      | ty == py && tx > px -> R
      | otherwise -> L
    _ -> U

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
