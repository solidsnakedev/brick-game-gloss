module Animation (
    initGameState,
    renderGame,
    updateState,
    handleKey,
    GameState (..),
    Env (..)
) where

import Animation.State
import Animation.Type
import Animation.Render