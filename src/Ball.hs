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

ballInitialVelocity :: V2 Float
ballInitialVelocity = V2 8 (-8)

makeBall :: V2 Float -> V2 Float -> Game ()
makeBall pos velocity = do
  gameState@GameState{..} <- get
  let
    radius = 20
    (sw, sh) = gameDimension
    V2 x y = pos
    model =
      mkTransformationMat (identity :: M33 Float) (V3 x y 0)
      !*! scaled (V4 radius radius 1 1)
    ball = Ball { ballPos = pos
                , ballRadius = radius
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

    check (V2 x y)
      | x <= 0 = (V2 (-1) 1 * ballVelocity, V2 0 y)
      | x + ballRadius >= sw = (V2 (-1) 1 * ballVelocity, V2 (sw - ballRadius) y)
      | y <= 0 = (V2 1 (-1) * ballVelocity, V2 x 0)
      | y + ballRadius >= sh = (V2 1 (-1) * ballVelocity, V2 x (sh - ballRadius))
      | otherwise = (ballVelocity, V2 x y)

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

checkBallHitBottom :: Game ()
checkBallHitBottom = do
  gameState@GameState{..} <- get
  let
    ball@Ball{..} = fromJust gameBall
    (sw, sh) = gameDimension
    (V2 x y) = ballPos
    (V2 dx dy) = ballVelocity
  when (y + dy + ballRadius >= sh) $
    put $ gameState { gameStatus = GameStopped
                    , gameBall = Just $ ball { ballVelocity = V2 0 0 }
                    }
