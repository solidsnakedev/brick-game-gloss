module Animation.Type (ObjectType (..), Object (..), DirectionX (..), DirectionY (..), GameState (..), Env (..), KeyStatus(..), GameStatus(..)) where

import qualified Data.Map as Map

data ObjectType = Ball | Brick deriving (Show)

data Object = Object
    { objectPosition :: (Int, Int)
    , objectType :: ObjectType
    }
    deriving (Show)

data KeyStatus = KeyLeftDown | KeyRightDown | NoKeyDown deriving (Show)

data DirectionY = GoUp | GoDown deriving (Show, Enum, Eq)

data DirectionX = GoLeft | GoRight deriving (Show, Enum, Eq)

data GameStatus = Playing | GameOver deriving (Show, Eq)

data GameState = GameState
    { directionY :: DirectionY
    , directionX :: DirectionX
    , objects :: Map.Map String Object
    , environment :: Env
    , boardPos :: (Int, Int)
    , collisions :: [String]
    , keyStatus :: KeyStatus
    , gameStatus :: GameStatus
    , debug :: String
    }
    deriving (Show)

data Env = Env
    { windowWidth :: Int
    , windowHeight :: Int
    , boardSize :: Int
    , ballRadious :: Int
    }
    deriving (Show)