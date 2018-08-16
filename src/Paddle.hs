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
    y = sh - sy
    model =
      mkTransformationMat (identity :: M33 Float) (V3 x y 0)
      !*! scaled (V4 sx sy 1 1)
    paddle = Paddle { paddlePos = V2 x y
                    , paddleSize = (sx, sy)
                    , paddleModel = model
                    }

  put gameState { gamePaddle = Just paddle }

updatePaddle :: Action -> Game ()
updatePaddle action = do
  gameState@GameState{..} <- get
  let
    (sw, sh) = gameDimension
    (V2 pos _) = paddlePos . fromJust $ gamePaddle
  case action of
    Move dx -> makePaddle (max 0 . min (sw - 100) $ pos + dx * 50)
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

makePaddleCollison :: Game ()
makePaddleCollison = do
  gameState@GameState{..} <- get
  when (checkCollison (fromJust gameBall) (fromJust gamePaddle)) $ do
    let
      ball@Ball{..} = fromJust gameBall
      Paddle{..} = fromJust gamePaddle
      V2 bx by = ballPos
      V2 bvx bvy = ballVelocity
      px = fst paddleSize
      V2 pos _ = paddlePos
      centerBoard = pos + px / 2
      distance = bx + ballRadius - centerBoard
      percentage = distance / (px / 2)
      v = V2 (3 * percentage * 2) (-1 * abs bvy)
      v' = signorm v ^* norm ballVelocity
    put $ gameState { gameBall = Just $ ball { ballVelocity = v' } }

checkCollison :: Ball -> Paddle -> Bool
checkCollison Ball{..} Paddle{..} =
  let
    V2 px1 py1 = ballPos
    sx1 = ballRadius
    sy1 = sx1
    V2 px2 py2 = paddlePos
    (sx2, sy2) = paddleSize
    collisionX = px1 + sx1 >= px2 && px2 + sx2 >= px1
    collisionY = py1 + sy1 >= py2 && py2 + sy2 >= py1
  in
    collisionX && collisionY
