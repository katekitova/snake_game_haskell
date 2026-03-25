module Logic
  ( initialState,
    startGame,
    startGameWithMode,
    restartGame,
    backToMenu,
    togglePause,
    stepGame,
    stepGameIO,
    moveSnake,
    advance,
    normalizeHead,
    hitsWall,
    hitsSelf,
    setDirection,
    spawnFood,
    isOpposite,
    loadBestScore,
    saveBestScore
  ) where

import System.Directory (doesFileExist)
import System.Random (StdGen, randomR)
import Text.Read (readMaybe)
import Types

bestScoreFile :: FilePath
bestScoreFile = "data/best_score.txt"

loadBestScore :: IO Int
loadBestScore = do
  exists <- doesFileExist bestScoreFile
  if not exists
    then pure 0
    else do
      content <- readFile bestScoreFile
      case readMaybe content of
        Just n -> pure n
        Nothing -> pure 0

saveBestScore :: Int -> IO ()
saveBestScore n = writeFile bestScoreFile (show n)

initialState :: StdGen -> GameState
initialState gen = GameState
  { snakeBody = [Cell (5, 5), Cell (4, 5), Cell (3, 5)],
    currentDir = R,
    pendingDir = R,
    foodCells = [Cell (10, 10)],
    boardW = 20,
    boardH = 20,
    cellSize = 30,
    gameStatus = Running,
    currentScreen = Menu,
    gameMode = Classic,
    isPaused = False,
    score = 0,
    bestScore = 0,
    tickTimer = 0,
    tickDelay = 0.22,
    rngGen = gen}

startGame :: GameState -> GameState
startGame gs = startGameWithMode (gameMode gs) gs

startGameWithMode :: GameMode -> GameState -> GameState
startGameWithMode mode gs = gs
    { snakeBody = [Cell (5, 5), Cell (4, 5), Cell (3, 5)],
      currentDir = R,
      pendingDir = R,
      foodCells =
        case mode of
          MultiFood -> [Cell (10, 10), Cell (14, 7), Cell (3, 12)]
          _ -> [Cell (10, 10)],
      gameStatus = Running,
      currentScreen = Playing,
      gameMode = mode,
      isPaused = False,
      score = 0,
      tickTimer = 0,
      tickDelay = 0.22}

restartGame :: GameState -> GameState
restartGame = startGame

togglePause :: GameState -> GameState
togglePause gs | currentScreen gs /= Playing = gs
  | gameStatus gs == GameOver = gs
  | otherwise = gs { isPaused = not (isPaused gs)}

normalizeHead :: GameState -> Cell -> Cell
normalizeHead gs (Cell (x, y)) =
  case gameMode gs of
    Classic -> Cell (x, y)
    MultiFood -> Cell (x, y)
    Wrap -> Cell (wrap x (boardW gs), wrap y (boardH gs))
  where
    wrap n size | n < 0  = size - 1
      | n >= size = 0
      | otherwise = n

stepGame :: Float -> GameState -> GameState
stepGame dt gs | currentScreen gs /= Playing = gs
  | gameStatus gs == GameOver = gs
  | isPaused gs = gs
  | otherwise = let newTimer = tickTimer gs + dt
      in if newTimer < tickDelay gs
           then gs { tickTimer = newTimer }
           else moveSnake gs { tickTimer = 0 }

stepGameIO :: Float -> GameState -> IO GameState
stepGameIO dt gs = do
  let newState = stepGame dt gs
  if bestScore newState > bestScore gs
    then saveBestScore (bestScore newState) >> pure newState
    else pure newState

moveSnake :: GameState -> GameState
moveSnake gs =
  let dir = pendingDir gs
      oldBody = snakeBody gs
      oldHead = head oldBody
      rawHead = advance dir oldHead
      newHead = normalizeHead gs rawHead
      ateFood = newHead `elem` foodCells gs
      candidateBody = if ateFood
          then newHead : oldBody
          else newHead : init oldBody
      wouldHitWall = hitsWall gs rawHead
      wouldHitSelf = hitsSelf candidateBody
  in if wouldHitWall || wouldHitSelf
      then gs
          { gameStatus = GameOver,
            bestScore = max (bestScore gs) (score gs)}
      else let movedState = gs
                { snakeBody = candidateBody,
                  currentDir = dir}
        in if ateFood
            then
              let remainingFoods = filter (/= newHead) (foodCells movedState)
                  stateWithoutFood = movedState { foodCells = remainingFoods }
                  (newFood, newGen) = spawnFood stateWithoutFood
                  newScore = score movedState + 1
                  newDelay = max 0.08 (tickDelay movedState * 0.985)
              in stateWithoutFood
                  { foodCells = newFood : remainingFoods,
                    rngGen = newGen,
                    score = newScore,
                    bestScore = max (bestScore movedState) newScore,
                    tickDelay = newDelay
                  }
            else movedState

advance :: Direction -> Cell -> Cell
advance U (Cell (x, y)) = Cell (x, y + 1)
advance D (Cell (x, y)) = Cell (x, y - 1)
advance L (Cell (x, y)) = Cell (x - 1, y)
advance R (Cell (x, y)) = Cell (x + 1, y)

hitsWall :: GameState -> Cell -> Bool
hitsWall gs (Cell (x, y)) = case gameMode gs of
    Classic -> x < 0 || y < 0 || x >= boardW gs || y >= boardH gs
    MultiFood -> x < 0 || y < 0 || x >= boardW gs || y >= boardH gs
    Wrap -> False

hitsSelf :: [Cell] -> Bool
hitsSelf [] = False
hitsSelf (h:rest) = h `elem` rest

setDirection :: Direction -> GameState -> GameState
setDirection newDir gs | currentScreen gs /= Playing = gs
  | gameStatus gs == GameOver = gs
  | isPaused gs = gs
  | isOpposite newDir (currentDir gs) = gs
  | otherwise = gs { pendingDir = newDir }

spawnFood :: GameState -> (Cell, StdGen)
spawnFood gs = findFreeCell (rngGen gs)
  where
    w = boardW gs
    h = boardH gs
    findFreeCell gen =
      let (x, gen1) = randomR (0, w - 1) gen
          (y, gen2) = randomR (0, h - 1) gen1
          cell = Cell (x, y)
      in if cell `elem` snakeBody gs || cell `elem` foodCells gs
           then findFreeCell gen2
           else (cell, gen2)

backToMenu :: GameState -> GameState
backToMenu gs = gs { currentScreen = Menu, isPaused = False }

isOpposite :: Direction -> Direction -> Bool
isOpposite U D = True
isOpposite D U = True
isOpposite L R = True
isOpposite R L = True
isOpposite _ _ = False