{-# LANGUAGE RecordWildCards #-}

module Ball where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Program
import           State

ballInitialVelocity :: V2 Float
ballInitialVelocity = V2 8 (-8)

makeBall :: V2 Float -> V2 Float -> Ball
makeBall pos velocity =
  Ball {ballPos = pos, ballRadius = 20, ballVelocity = velocity}

updateBall :: (Float, Float) -> Ball -> Ball
updateBall (sw, sh) Ball{..} = do
  let (updateVelocity, updatedPos) = check $ ballPos + ballVelocity
      check (V2 x y)
        | x <= 0 = (V2 (-1) 1 * ballVelocity, V2 0 y)
        | x + ballRadius >= sw =
          (V2 (-1) 1 * ballVelocity, V2 (sw - ballRadius) y)
        | y <= 0 = (V2 1 (-1) * ballVelocity, V2 x 0)
        | y + ballRadius >= sh =
          (V2 1 (-1) * ballVelocity, V2 x (sh - ballRadius))
        | otherwise = (ballVelocity, V2 x y)
  makeBall updatedPos updateVelocity

renderBall :: Game ()
renderBall = do
  GameState {..} <- get
  let Mesh {..} = gameMeshes Map.! "block"
      tex = gameTextures Map.! "awesomeface"
      Ball {..} = gameBall
      ballModel =
        mkTransformationMat
          (identity :: M33 Float)
          (V3 (ballPos ^. _x) (ballPos ^. _y) 0) !*!
        scaled (V4 ballRadius ballRadius 1 1)
  (Just program) <- GL.get GL.currentProgram
  liftIO $ do
    glModelMatrix <- toGlMatrix ballModel
    setUniform program "model" glModelMatrix
    setUniform program "blockColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)
    GL.textureBinding GL.Texture2D $= Just tex
    GL.bindVertexArrayObject $= Just meshVAO
    GL.drawArrays GL.Triangles 0 meshLength

ballHitBottom :: (Float, Float) -> Ball -> Ball
ballHitBottom (_, h) ball@Ball{..} =
  if (ballPos ^. _y) + (ballVelocity ^. _y) + ballRadius >= h
  then ball {ballVelocity = V2 0 0}
  else ball
