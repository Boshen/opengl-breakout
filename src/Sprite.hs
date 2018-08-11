{-# LANGUAGE RecordWildCards #-}

module Sprite where

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Foldable              as Foldable
import qualified Data.Map.Strict            as Map
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))

import           Textures
import           Program
import           State

makeSprites :: Game ()
makeSprites = makeSprite (V3 200 200 0)

makeSprite :: V3 Int -> Game ()
makeSprite pos = do
  gameState@GameState{..} <- get

  case Map.lookup pos gameSprites of
    Just sprite -> return ()
    Nothing -> do
      let
        rotate = axisAngle (V3 0 0 (1)) (pi / 4)
        sx = 300
        sy = 400
        model =
          mkTransformationMat (identity :: M33 Float) (fromIntegral <$> pos)
          !*! mkTransformationMat (identity :: M33 Float) (V3 (sx / 2) (sy / 2) 0)
          !*! mkTransformation rotate (V3 (-1 * sx / 2) (-1 * sy / 2) 0)
          !*! scaled (V4 sx sy 1 1)

        sprite = Sprite { spritePos = pos
                        , spriteVAO = Nothing
                        , spriteVBO = Nothing
                        , spriteModel = model
                        , spriteDirty = True
                        }
      put gameState { gameSprites = Map.insert pos sprite gameSprites }

renderSprites :: Game ()
renderSprites = do
  GameState{..} <- get

  -- setup program
  let program = getSpriteProgram gamePrograms
  GL.currentProgram $= Just program

  -- set uniforms
  liftIO $ do
    glProjectionMatrix <- toGlMatrix $ ortho 0 800 600 0 (-1) 1
    setUniform program "projection" glProjectionMatrix


  -- render each sprite
  mapM_ renderSprite gameSprites

renderSprite :: Sprite -> Game ()
renderSprite sprite@Sprite{..} = do
  gameState@GameState{..} <- get

  (Just program) <- GL.get GL.currentProgram

  -- reposition and rescale each srpite
  liftIO $ do
    glModelMatrix <- toGlMatrix spriteModel
    setUniform program "model" glModelMatrix
    setUniform program "spriteColor" (GL.Vertex3 0 1 0 :: GL.Vertex3 Float)

  -- redraw the sprite if its not updated
  -- otherwise create indices for drawing
  if not spriteDirty
  then
    liftIO $ do
      GL.bindVertexArrayObject $= spriteVAO
      GL.drawArrays GL.Triangles 0 6
  else do
    vao <- GL.genObjectName
    vbo <- GL.genObjectName

    liftIO $ do
      GL.bindVertexArrayObject $= Just vao
      GL.bindBuffer GL.ArrayBuffer $= Just vbo

      withArray vertices $ \ptr ->
        GL.bufferData GL.ArrayBuffer $= (fromIntegral $ length vertices * 4, ptr, GL.StaticDraw)

      GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
      GL.vertexAttribPointer (GL.AttribLocation 0) $=
        (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 16 (intPtrToPtr 0))

      GL.drawArrays GL.Triangles 0 6

      tx <- loadTex "awesomeface.png"
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
      texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
      GL.texture GL.Texture2D $= GL.Enabled
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tx

    -- no need to recreate buffer next time
    let updatedSprite = sprite { spriteVAO = Just vao
                                , spriteVBO = Just vbo
                                , spriteDirty = False
                                }
    put $ gameState { gameSprites = Map.insert spritePos updatedSprite gameSprites }

floatSize = fromIntegral . sizeOf $ (0 :: Float)

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)

vertices :: [Float]
vertices =
  -- Pos, Tex
  [ 0, 1, 0, 1
  , 1, 0, 1, 0
  , 0, 0, 0, 0

  , 0, 1, 0, 1
  , 1, 1, 1, 1
  , 1, 0, 1, 0
  ]
