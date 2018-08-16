module Mesh where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Foreign.Marshal.Array
import           Foreign.Ptr
import qualified Graphics.Rendering.OpenGL  as GL
import           Graphics.Rendering.OpenGL (($=))

import           State

buildMeshes :: IO (Map String Mesh)
buildMeshes = do
  blockMesh <- buildBlockMesh
  return $ Map.fromList [("block", blockMesh)]

buildBlockMesh :: IO Mesh
buildBlockMesh = do
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

    return $ Mesh { meshVAO = vao
                  , meshVBO = vbo
                  , meshLength = 6
                  }

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
