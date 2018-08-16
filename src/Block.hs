{-# LANGUAGE RecordWildCards #-}

module Block where

import           Control.Monad.State.Strict
import           Data.Foldable              (maximumBy)
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           Data.Ord                   (comparing)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Program
import           State
import           Textures

width, height :: Int
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
      x = fromIntegral i * sw / fromIntegral width
      y = fromIntegral j * sh / (4 * fromIntegral height)
      sx = sw / fromIntegral width
      sy = sh / fromIntegral height / 4
      rotate = axisAngle (V3 0 0 1) 0

      model =
        mkTransformationMat (identity :: M33 Float) (V3 x y 0)
        !*! mkTransformationMat (identity :: M33 Float) (V3 (sx / 2) (sy / 2) 0)
        !*! mkTransformation rotate (V3 (-1 * sx / 2) (-1 * sy / 2) 0)
        !*! scaled (V4 sx sy 1 1)

      block = Block { blockPos = V2 x y
                    , blockSize = (sx, sy)
                    , blockLevel = level
                    , blockModel = model
                    }
    put gameState { gameBlocks = Map.insert (V2 x y) block gameBlocks }

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

makeBlockCollison :: Game ()
makeBlockCollison = do
  gameState@GameState{..} <- get
  let
    ball@Ball{..} = fromJust gameBall
  forM_ gameBlocks $ \block -> do
    let
      (collision, dir, V2 dx dy) = checkCollison ball block
    when collision $ do
      let
        (dp, dv) = if dir == LEFT || dir == RIGHT
                  then (V2 (ballRadius - abs dx) 0, V2 (-1) 1)
                  else (V2 0 (ballRadius - abs dy), V2 1 (-1))
        updatedBall = ball { ballPos = ballPos + dp
                          , ballVelocity = ballVelocity * dv
                          }
      put $ gameState { gameBall = Just updatedBall
                      , gameBlocks = Map.delete (blockPos block) gameBlocks
                      }

clamp :: V2 Float -> V2 Float -> V2 Float -> V2 Float
clamp (V2 x y) (V2 minx miny) (V2 maxx maxy) = V2 (min maxx . max minx $ x) (min maxy . max miny $ y)

checkCollison :: Ball -> Block -> (Bool, Direction, V2 Float)
checkCollison Ball{..} Block{..} =
  let
    center = ballPos ^+^ V2 ballRadius ballRadius -- 60 600
    aabbHalfExtents = let (x, y) = blockSize in V2 x y ^/ 2 -- 133 50
    aabbCenter = blockPos + aabbHalfExtents -- 133 150
    difference = center - aabbCenter -- -73 450
    clamped = clamp difference (-1 *^ aabbHalfExtents) aabbHalfExtents
    closest = aabbCenter + clamped
    diff = closest - center
  in
    if norm diff <= ballRadius
    then (True, vectorDirection diff, diff)
    else (False, UP, V2 0 0)

vectorDirection :: V2 Float -> Direction
vectorDirection target =
  let
    dir = [UP, RIGHT, DOWN, LEFT]
    compass = [ V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0 ]
  in
    fst . maximumBy (comparing snd) . zip dir $ map (`dot` signorm target) compass
