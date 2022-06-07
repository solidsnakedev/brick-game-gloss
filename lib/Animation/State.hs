module Animation.State (initGameState, updateState, handleKey, detectCollisionY) where

import Animation.Env (initEnv)
import Animation.Type (DirectionX (..), DirectionY (..), Env (..), GameState (..), GameStatus (..), KeyStatus (..), Object (..), ObjectType (..))
import qualified Data.Map as Map
import System.Random (randomRIO)

import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down), SpecialKey (KeyDown, KeyEsc, KeyLeft, KeyRight, KeyUp))
import System.Exit (exitSuccess)

handleKey :: Event -> GameState -> IO GameState
handleKey (EventKey (SpecialKey key) Down _ _) game =
    case key of
        KeyRight -> return game{keyStatus = KeyRightDown}
        KeyLeft -> return game{keyStatus = KeyLeftDown}
        KeyEsc -> exitSuccess
        _ -> return game
  where
    offset = 5
    (boardPosX, boardPosY) = boardPos game
handleKey (EventKey (Char 'r') _ _ _) game = initGameState
handleKey _ game = return game{keyStatus = NoKeyDown}

getRandPos :: IO (Int, Int)
getRandPos = do
    randX <- randomRIO (1, 300) :: IO Int
    randY <- randomRIO (1, 150) :: IO Int
    return (randX, randY)

mkRandObjects :: IO (Map.Map String Object)
mkRandObjects = do
    brickList <- mapM (\_ -> getRandPos) [1 .. 200]
    let ob = map (\x -> Object x Brick) brickList
    let bricks = map (\object -> ("brick" ++ show (objectPosition object), object)) ob
    ballPos <- getRandPos
    let ball = Object ballPos Ball
    let objects = Map.fromList $ ("ball", ball) : bricks
    print objects
    return objects

initGameState :: IO GameState
initGameState = do
    objects <- mkRandObjects
    return $
        GameState
            { directionY = GoDown
            , directionX = GoLeft
            , objects = objects
            , environment = initEnv
            , boardPos = (0, 300)
            , collisions = []
            , keyStatus = NoKeyDown
            , gameStatus = Playing
            , debug = ""
            }

updateState :: Float -> GameState -> IO GameState
updateState _ (GameState directionY directionX objects env (boardPosX, boardPosY) collisions keyStatus gameStatus debug) = return $ GameState newDirectionY newDirectionX newObjects env (boardPosX', boardPosY) newCollisions keyStatus newGameStatus newDebug
  where
    isCollision = not (null collisions)
    boardSteps = 1
    boardPosX' = case keyStatus of
        KeyRightDown -> boardPosX + boardSteps
        KeyLeftDown -> boardPosX - boardSteps
        NoKeyDown -> boardPosX

    (ballPosX, ballPosY) = maybe (error "ball is missing") objectPosition (Map.lookup "ball" objects)
    isInsideBoard = boardPosX' <= ballPosX && ballPosX <= (boardPosX' + boardSize env)
    isContactTopBoard = ballPosY + ballRadious env == boardPosY
    isCollisionBoard = isInsideBoard && isContactTopBoard
    newDirectionY = calcDirectionY directionY ballPosY (windowHeight env) isCollisionBoard isCollision
    newDirectionX = calcDirectionX directionX ballPosX (windowWidth env)
    newballPosY = calcBallPosY newDirectionY ballPosY
    newballPosX = calcBallPosX newDirectionX ballPosX

    -- Detect collision from Objects
    newCollisions = Map.keys $ Map.filterWithKey (\key value -> key /= "ball" && (newballPosX == (fst $ objectPosition value)) && detectCollisionY newballPosY 3 (snd $ objectPosition value)) objects
    -- If collision then delete Brick
    newObjectsMap = deleteBricks newCollisions objects

    -- Create ball with new positions
    newBall = Object (newballPosX, newballPosY) Ball
    -- Update ball positions
    newObjects = Map.adjust (\_ -> newBall) "ball" newObjectsMap
    newGameStatus = if ballPosY > boardPosY then GameOver else Playing
    newDebug = mconcat [show $ (boardPosX, boardPosY), " ", show $ (ballPosX, ballPosY), "", show isInsideBoard, show $ isCollisionBoard]

calcDirectionY :: (Ord a, Num a) => DirectionY -> a -> a -> Bool -> Bool -> DirectionY
calcDirectionY directionY ballPosY row isCollisionBoard isCollision =
    case directionY of
        GoUp -> if ballPosY <= 0 || isCollision then GoDown else GoUp
        GoDown -> if isCollisionBoard || isCollision then GoUp else GoDown

calcDirectionX :: (Ord a, Num a) => DirectionX -> a -> a -> DirectionX
calcDirectionX directionX ballPosX column =
    case directionX of
        GoLeft -> if ballPosX <= 1 then GoRight else GoLeft
        GoRight -> if ballPosX >= column then GoLeft else GoRight

calcBallPosX :: Num a => DirectionX -> a -> a
calcBallPosX directionX ballPosX = if directionX == GoRight then ballPosX + 1 else abs (ballPosX - 1)

calcBallPosY :: Num a => DirectionY -> a -> a
calcBallPosY directionY ballPosY = if directionY == GoDown then ballPosY + 1 else abs (ballPosY - 1)

deleteBricks :: Ord k => [k] -> Map.Map k a -> Map.Map k a
deleteBricks [] obMap = obMap
deleteBricks (x : xs) obMap = deleteBricks xs (Map.delete x obMap)

detectCollisionY ballPosY ballRadius objectPosY = ballPosY + ballRadius >= objectPosY && ballPosY - ballRadius <= objectPosY