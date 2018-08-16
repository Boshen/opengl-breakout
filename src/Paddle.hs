{-# LANGUAGE RecordWildCards #-}

module Paddle where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Program
import           State

makePaddle :: (Float, Float) -> Float -> Paddle
makePaddle (_, sh) x = do
  let paddleSize = V2 100 20
      y = sh - (paddleSize ^. _y)
  Paddle {paddlePos = V2 x y, paddleSize = paddleSize}

updatePaddle :: Action -> Game ()
updatePaddle action = do
  gameState@GameState {..} <- get
  let (sw, _) = gameDimension
      (V2 pos _) = paddlePos gamePaddle
  case action of
    Move dx -> do
      let paddle =
            makePaddle gameDimension (max 0 . min (sw - 100) $ pos + dx * 50)
      put gameState {gamePaddle = paddle}
    _ -> return ()

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

makePaddleCollison :: Game ()
makePaddleCollison = do
  gameState@GameState {..} <- get
  when (checkCollison gameBall gamePaddle) $ do
    let ball@Ball {..} = gameBall
        Paddle {..} = gamePaddle
        centerBoard = (paddlePos ^. _x) + paddleSize ^. _x / 2
        dist = (ballPos ^. _x) + ballRadius - centerBoard
        percentage = dist / (paddleSize ^. _x / 2)
        v = V2 (3 * percentage * 2) (-1 * abs (ballVelocity ^. _y))
        v' = signorm v ^* norm ballVelocity
    put $ gameState {gameBall = ball {ballVelocity = v'}}

checkCollison :: Ball -> Paddle -> Bool
checkCollison Ball {..} Paddle {..} =
  let V2 px1 py1 = ballPos
      V2 px2 py2 = paddlePos
      V2 sx2 sy2 = paddleSize
      collisionX = px1 + ballRadius >= px2 && px2 + sx2 >= px1
      collisionY = py1 + ballRadius >= py2 && py2 + sy2 >= py1
   in collisionX && collisionY
