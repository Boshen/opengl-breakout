{-# LANGUAGE RecordWildCards #-}

module Ball where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Program
import           State

ballRadius :: Float
ballRadius = 20

ballInitialVelocity :: V3 Float
ballInitialVelocity = V3 5 (-5) 0

makeBall :: V3 Float -> V3 Float -> Game ()
makeBall pos velocity = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = gameDimension
    model =
      mkTransformationMat (identity :: M33 Float) pos
      !*! scaled (V4 ballRadius ballRadius 1 1)
    ball = Ball { ballPos = pos
                , ballVelocity = velocity
                , ballModel = model
                }

  put gameState { gameBall = Just ball }

updateBall :: Float -> Game ()
updateBall dt = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = gameDimension
    ball@Ball{..} = fromJust gameBall
    (updateVelocity, updatedPos) = check $ ballPos + ballVelocity

    check (V3 x y z)
      | x <= 0 = (V3 (-1) 1 1 * ballVelocity, V3 0 y z)
      | x + ballRadius >= sw = (V3 (-1) 1 1 * ballVelocity, V3 (sw - ballRadius) y z)
      | y <= 0 = (V3 1 (-1) 1 * ballVelocity, V3 x 0 z)
      | y + ballRadius >= sh = (V3 1 (-1) 1 * ballVelocity, V3 x (sh - ballRadius) z)
      | otherwise = (ballVelocity, V3 x y z)

  makeBall updatedPos updateVelocity

renderBall :: Game ()
renderBall = do
  gameState@GameState{..} <- get
  let
    Mesh{..} = gameMeshes Map.! "block"
    tex = gameTextures Map.! "awesomeface"
    Ball{..} = fromJust gameBall

  (Just program) <- GL.get GL.currentProgram
  liftIO $ do
    glModelMatrix <- toGlMatrix ballModel
    setUniform program "model" glModelMatrix
    setUniform program "blockColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)

    GL.textureBinding GL.Texture2D $= Just tex
    GL.bindVertexArrayObject $= Just meshVAO
    GL.drawArrays GL.Triangles 0 meshLength
