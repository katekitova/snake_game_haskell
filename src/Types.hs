module Types where

import System.Random (StdGen)

data Direction
  = U
  | D
  | L
  | R
  deriving (Show, Eq)

newtype Cell = Cell (Int, Int)
  deriving (Show, Eq)

data Status = Running
  | GameOver
  deriving (Show, Eq)

data Screen = Menu
  | Playing
  deriving (Show, Eq)

data GameMode = Classic
  | Wrap
  | MultiFood
  deriving (Show, Eq)

data GameState = GameState
  { snakeBody :: [Cell],
    currentDir :: Direction,
    pendingDir :: Direction,
    foodCells :: [Cell],
    boardW :: Int,
    boardH :: Int,
    cellSize :: Float,
    gameStatus :: Status,
    currentScreen :: Screen,
    gameMode :: GameMode,
    isPaused :: Bool,
    score :: Int,
    bestScore :: Int,
    tickTimer :: Float,
    tickDelay :: Float,
    rngGen :: StdGen,
    eatAnimTimer :: Float}
  deriving (Show)