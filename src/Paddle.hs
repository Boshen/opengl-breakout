{-# LANGUAGE RecordWildCards #-}

module Paddle where

import Data.Maybe
import qualified Graphics.Rendering.OpenGL  as GL
import qualified Data.Map.Strict            as Map
import           Control.Monad.State.Strict
import           Linear
import           SDL                        (($=))

import           Program
import           State

makePaddle :: Game ()
makePaddle = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = fromIntegral <$> gameDimension
    sx = 100
    sy = 20
    model =
      mkTransformationMat (identity :: M33 Float) (V3 0 (sh - sy) 0)
      !*! scaled (V4 sx sy 1 1)
    paddle = Paddle { paddlePos = sh
                    , paddleModel = model
                    }

  put gameState { gamePaddle = Just paddle }

renderPaddle :: Game ()
renderPaddle = do
  gameState@GameState{..} <- get
  let
    Mesh{..} = gameMeshes Map.! "block"
    tex = gameTextures Map.! "paddle"
    Paddle{..} = fromJust gamePaddle

  (Just program) <- GL.get GL.currentProgram
  liftIO $ do
    glModelMatrix <- toGlMatrix paddleModel
    setUniform program "model" glModelMatrix
    setUniform program "blockColor" (GL.Vertex3 1 1 1 :: GL.Vertex3 Float)

    GL.textureBinding GL.Texture2D $= Just tex
    GL.bindVertexArrayObject $= Just meshVAO
    GL.drawArrays GL.Triangles 0 meshLength
