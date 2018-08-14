{-# LANGUAGE RecordWildCards #-}

module Paddle where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Program
import           State

makePaddle :: Float -> Game ()
makePaddle x = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = gameDimension
    sx = 100
    sy = 20
    model =
      mkTransformationMat (identity :: M33 Float) (V3 x (sh - sy) 0)
      !*! scaled (V4 sx sy 1 1)
    paddle = Paddle { paddlePos = x
                    , paddleModel = model
                    }

  put gameState { gamePaddle = Just paddle }

updatePaddle :: Action -> Game ()
updatePaddle action = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = gameDimension
    pos = paddlePos . fromJust $ gamePaddle
  case action of
    Move dx -> makePaddle (max 0 . min (sw - 100) $ pos + dx * 15)
    _       -> return ()

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
