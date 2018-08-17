{-# LANGUAGE RecordWildCards #-}

module Block where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Foldable              (maximumBy)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Ord                   (comparing)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Collidable
import           Program
import           State

width, height :: Int
width = 6
height = 3

blocks :: Vector Int
blocks = V.fromList
  [ 1, 1, 1, 1, 1, 1
  , 2, 2, 0, 0, 2, 2
  , 3, 3, 4, 4, 3, 3
  ]

makeBlocks :: (Float, Float) -> Map (V2 Float) Block
makeBlocks dimen = Map.fromList $ map (makeBlock dimen) [(i, j) | i <- [0..width-1], j <- [0..height-1]]

makeBlock :: (Float, Float) -> (Int, Int) -> (V2 Float, Block)
makeBlock gameDimension (i, j) =
  let level = blocks V.! (j * width + i)

      (sw, sh) = gameDimension
      x = fromIntegral i * sw / fromIntegral width
      y = fromIntegral j * sh / (4 * fromIntegral height)
      sx = sw / fromIntegral width
      sy = sh / fromIntegral height / 4

      blk = Block { blockPos = V2 x y
                    , blockSize = V2 sx sy
                    , blockLevel = level
                    }
  in
    (V2 x y, blk)

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
renderBlock Block{..} = do
  GameState{..} <- get
  let
    Mesh{..} = gameMeshes Map.! "block"
    tex = gameTextures Map.! "block"
    rot = axisAngle (V3 0 0 1) 0
    blockModel =
      mkTransformationMat (identity :: M33 Float) (V3 (blockPos^._x) (blockPos^._y) 0)
      !*! mkTransformationMat (identity :: M33 Float) (V3 (blockSize^._x / 2) (blockSize^._y / 2) 0)
      !*! mkTransformation rot (V3 (-1 * (blockSize^._x) / 2) (-1 * (blockSize^._y) / 2) 0)
      !*! scaled (V4 (blockSize^._x) (blockSize^._y) 1 1)

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
    ball@Ball{..} = gameBall
  forM_ gameBlocks $ \blk -> do
    let
      (collision, dir, V2 dx dy) = checkCollison ball blk
    when collision $ do
      let
        (dp, dv) = if dir == LEFT || dir == RIGHT
                  then (V2 (ballRadius - abs dx) 0, V2 (-1) 1)
                  else (V2 0 (ballRadius - abs dy), V2 1 (-1))
        updatedBall = ball { ballPos = ballPos + dp
                          , ballVelocity = ballVelocity * dv
                          }
      put $ gameState { gameBall = updatedBall
                      , gameBlocks = Map.delete (blockPos blk) gameBlocks
                      }

clamp :: V2 Float -> V2 Float -> V2 Float -> V2 Float
clamp (V2 x y) (V2 minx miny) (V2 maxx maxy) = V2 (min maxx . max minx $ x) (min maxy . max miny $ y)

checkCollison :: (Collidable a, Collidable b) => a -> b -> (Bool, Direction, V2 Float)
checkCollison a b =
  let
    center = pos a ^+^ size a
    aabbHalfExtents = size b / 2
    aabbCenter = pos b + aabbHalfExtents
    difference = center - aabbCenter
    clamped = clamp difference (-1 *^ aabbHalfExtents) aabbHalfExtents
    closest = aabbCenter + clamped
    diff = closest - center
  in
    if norm diff <= size a ^._x
    then (True, vectorDirection diff, diff)
    else (False, UP, V2 0 0)

vectorDirection :: V2 Float -> Direction
vectorDirection target =
  let
    dir = [UP, RIGHT, DOWN, LEFT]
    compass = [ V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0 ]
  in
    fst . maximumBy (comparing snd) . zip dir $ map (`dot` signorm target) compass
