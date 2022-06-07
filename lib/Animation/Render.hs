module Animation.Render (renderGame) where

import Animation.Type (Env (boardSize, windowHeight, windowWidth), GameState (..), GameStatus (..), Object (..), ObjectType (..))
import qualified Data.Map as Map
import Graphics.Gloss (Picture (Circle, Text), color, pictures, rectangleSolid, red, rotate, scale, translate, white)

renderGame :: GameState -> IO Picture
renderGame game = return $ if gameStatus game == Playing then pictures $ mconcat [[board], pic, [debugData]] else gameOver
 where
  debugData = color white $ translate (-140) (-130) $ scale 0.08 0.08 $ Text $ debug game
  gameOver = color white $ translate (-40) (0) $ scale 0.1 0.1 $ Text "Game Over !!"
  bSize = boardSize $ environment game
  offsetHeight = fromIntegral $ (windowHeight $ environment game) `div` 2
  offsetWidth = fromIntegral $ (windowWidth $ environment game) `div` 2
  offsetBoard = fromIntegral $ bSize `div` 2
  board = color red $ translate (fromIntegral (fst $ boardPos game) + offsetBoard - offsetWidth) (offsetHeight - fromIntegral (snd $ boardPos game)) $ rectangleSolid (fromIntegral bSize) 1
  objectElements = Map.elems $ objects game
  pic =
    map
      ( \(Object (posX, posY) objectType) -> case objectType of
          Ball -> color white $ translate (fromIntegral posX - offsetWidth) (offsetHeight - fromIntegral posY) (Circle 3)
          Brick -> color white $ translate (fromIntegral posX - offsetWidth) (offsetHeight - fromIntegral posY) (rectangleSolid 5 5)
      )
      objectElements
