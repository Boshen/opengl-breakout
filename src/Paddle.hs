{-# LANGUAGE RecordWildCards #-}

module Paddle where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable
import qualified Data.Map.Strict            as Map
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Collidable
import           Program
import           State

makePaddle :: (Float, Float) -> Float -> Paddle
makePaddle (_, sh) x = do
  let paddleSize = V2 100 20
      y = sh - (paddleSize ^. _y)
  Paddle {paddlePos = V2 x y, paddleSize = paddleSize}

updatePaddle :: (Float, Float) -> Paddle -> [Action] -> Paddle
updatePaddle gameDimension = foldl' updatePaddle'
  where
    updatePaddle' paddle action =
      case action of
        Move dx ->
          let (sw, _) = gameDimension
              (V2 pos _) = paddlePos paddle
           in makePaddle gameDimension (max 0 . min (sw - 100) $ pos + dx * 50)
        _ -> paddle

renderPaddle :: Game ()
renderPaddle = do
  GameState {..} <- get
  let Mesh {..} = gameMeshes Map.! "block"
      tex = gameTextures Map.! "paddle"
      Paddle {..} = gamePaddle
      paddleModel =
        mkTransformationMat
          (identity :: M33 Float)
          (V3 (paddlePos ^. _x) (paddlePos ^. _y) 0) !*!
        scaled (V4 (paddleSize ^. _x) (paddleSize ^. _y) 1 1)
  (Just program) <- GL.get GL.currentProgram
  liftIO $ do
    glModelMatrix <- toGlMatrix paddleModel
    setUniform program "model" glModelMatrix
    setUniform program "blockColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.bindVertexArrayObject $= Just meshVAO
    GL.drawArrays GL.Triangles 0 meshLength

makePaddleCollison :: Paddle -> Ball -> Ball
makePaddleCollison paddle@Paddle {..} ball@Ball {..} =
  if checkCollison ball paddle
    then let centerBoard = (paddlePos ^. _x) + paddleSize ^. _x / 2
             dist = (ballPos ^. _x) + ballRadius - centerBoard
             percentage = dist / (paddleSize ^. _x / 2)
             v = V2 (3 * percentage * 2) (-1 * abs (ballVelocity ^. _y))
             v' = signorm v ^* norm ballVelocity
          in ball {ballVelocity = v'}
    else ball

checkCollison :: (Collidable a, Collidable b) => a -> b -> Bool
checkCollison a b =
  let V2 px1 py1 = pos a
      V2 px2 py2 = pos b
      V2 sx1 sy1 = size a
      V2 sx2 sy2 = size b
      collisionX = px1 + sx1 >= px2 && px2 + sx2 >= px1
      collisionY = py1 + sy1 >= py2 && py2 + sy2 >= py1
   in collisionX && collisionY
