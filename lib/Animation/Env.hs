module Animation.Env (initEnv) where

import Animation.Type (Env (..))

initEnv :: Env
initEnv =
    Env
        { windowWidth = 300
        , windowHeight = 300
        , boardSize = 30
        , ballRadious = 3
        }