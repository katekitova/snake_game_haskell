module Input (handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

import Logic
import Types

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeyEsc) Down _ _) gs
    | currentScreen gs == Playing = do
        playMenuMusic
        pure (backToMenu gs)
    | otherwise = pure gs
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gs
    | currentScreen gs == Menu = do
        playGameMusic
        pure (startGame gs)
    | otherwise = pure gs
handleInput (EventKey (Char '1') Down _ _) gs
    | currentScreen gs == Menu = pure (gs { gameMode = Classic })
    | otherwise = pure gs
handleInput (EventKey (Char '2') Down _ _) gs
    | currentScreen gs == Menu = pure (gs { gameMode = Wrap })
    | otherwise = pure gs
handleInput (EventKey (Char '3') Down _ _) gs
    | currentScreen gs == Menu = pure (gs { gameMode = MultiFood })
    | otherwise = pure gs
handleInput (EventKey (SpecialKey KeyUp) Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (setDirection U gs)
    | otherwise = pure gs
handleInput (EventKey (SpecialKey KeyDown) Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (setDirection D gs)
    | otherwise = pure gs
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (setDirection L gs)
    | otherwise = pure gs
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (setDirection R gs)
    | otherwise = pure gs
handleInput (EventKey (Char 'p') Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (togglePause gs)
    | otherwise = pure gs
handleInput (EventKey (Char 'P') Down _ _) gs
    | currentScreen gs == Playing && gameStatus gs == Running = pure (togglePause gs)
    | otherwise = pure gs
handleInput (EventKey (Char 'r') Down _ _) gs
    | currentScreen gs == Playing = do
        playGameMusic
        pure (restartGame gs)
    | otherwise = pure gs
handleInput (EventKey (Char 'R') Down _ _) gs
    | currentScreen gs == Playing = do
        playGameMusic
        pure (restartGame gs)
    | otherwise = pure gs
handleInput (EventKey (Char 'q') Down _ _) _ = do
    stopAllMusic
    exitSuccess
handleInput (EventKey (Char 'Q') Down _ _) _ = do
    stopAllMusic
    exitSuccess
handleInput _ gs = pure gs
