{-# LANGUAGE RecordWildCards #-}

module Block where

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Program
import           State
import           Textures

width = 6
height = 3

blocks :: Vector Int
blocks = V.fromList
  [ 1, 1, 1, 1, 1, 1
  , 2, 2, 0, 0, 2, 2
  , 3, 3, 4, 4, 3, 3
  ]

makeBlocks :: Game ()
makeBlocks = mapM_ makeBlock [(i, j) | i <- [0..width-1], j <- [0..height-1]]

makeBlock :: (Int, Int) -> Game ()
makeBlock (i, j) = do
  gameState@GameState{..} <- get
  let
    level = blocks V.! (j * width + i)

  when (level > 0) $ do

    let
      (sw, sh) = gameDimension
      pos = V3 (i * sw `div` width) (j * sh `div` (4 * height)) 0
      sx = fromIntegral sw / fromIntegral width
      sy = fromIntegral sh / fromIntegral height / 4
      rotate = axisAngle (V3 0 0 1) 0

      model =
        mkTransformationMat (identity :: M33 Float) (fromIntegral <$> pos)
        !*! mkTransformationMat (identity :: M33 Float) (V3 (sx / 2) (sy / 2) 0)
        !*! mkTransformation rotate (V3 (-1 * sx / 2) (-1 * sy / 2) 0)
        !*! scaled (V4 sx sy 1 1)

      block = Block { blockPos = pos
                      , blockLevel = level
                      , blockModel = model
                      }
    put gameState { gameBlocks = Map.insert pos block gameBlocks }

renderBlocks :: Game ()
renderBlocks = do
  GameState{..} <- get

  let program = getBlockProgram gamePrograms
  GL.currentProgram $= Just program

  liftIO $ do
    glProjectionMatrix <- toGlMatrix $ ortho 0 800 600 0 (-1) 1
    setUniform program "projection" glProjectionMatrix

  mapM_ renderBlock gameBlocks

blockColor :: Int -> GL.Vertex3 Float
blockColor 5 = GL.Vertex3 1 0.5 0
blockColor 4 = GL.Vertex3 0.8 0.8 0.4
blockColor 3 = GL.Vertex3 0 0.7 0
blockColor 2 = GL.Vertex3 0.2 0.6 1
blockColor _ = GL.Vertex3 1 1 1

renderBlock :: Block -> Game ()
renderBlock block@Block{..} = do
  gameState@GameState{..} <- get
  let
    Mesh{..} = gameMeshes Map.! "block"
    tex = gameTextures Map.! "block"

  (Just program) <- GL.get GL.currentProgram

  liftIO $ do
    glModelMatrix <- toGlMatrix blockModel
    setUniform program "model" glModelMatrix
    setUniform program "blockColor" (blockColor blockLevel)

    GL.textureBinding GL.Texture2D $= Just tex
    GL.bindVertexArrayObject $= Just meshVAO
    GL.drawArrays GL.Triangles 0 meshLength
