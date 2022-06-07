module Main where

import Data.Text ()

import Animation (Env (..), GameState (..), handleKey, initGameState, renderGame, updateState)

import Graphics.Gloss (
    Display (InWindow),
    Picture (Circle, Pictures),
    circle,
    color,
    display,
    pictures,
    play,
    rectangleSolid,
    translate,
    white,
 )
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game (playIO)

fps = 60

main :: IO ()
main = do
    (screenX, screenY) <- getScreenSize
    initGameState' <- initGameState
    let height = windowHeight $ environment initGameState'
    let width = windowWidth $ environment initGameState'
    let window = InWindow "Game" (height, width) (screenX `div` 2 - width `div` 2, screenY `div` 2 - height `div` 2)
    playIO window black fps initGameState' renderGame handleKey updateState
