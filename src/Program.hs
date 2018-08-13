module Program where

import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Control.Monad
import qualified Data.Foldable              as Foldable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Graphics.Rendering.OpenGL as GL
import           SDL                       (($=))
import           Linear

import           JuicyTextures
import           Textures
import           LoadShaders

block = "block"

programs :: [(String, String, String)]
programs = [(block, "./shaders/block.vert", "./shaders/block.frag")]

uniforms :: [(String, GL.AttribLocation)]
uniforms = map
  (\(i, name) -> (name, GL.AttribLocation i))
  (zip [0..] ["model", "projection", "image", "blockColor"])

buildPrograms :: IO (Map String GL.Program)
buildPrograms = Map.fromList <$> mapM buildProgram programs

buildProgram :: (String, String, String) -> IO (String, GL.Program)
buildProgram (name, vert, frag) = do
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource vert)
      , ShaderInfo GL.FragmentShader (FileSource frag)
      ]
  mapM_ (\(name, loc) -> GL.attribLocation program name $= loc) uniforms
  return (name, program)

setUniform program name d = do
  location <- GL.uniformLocation program name
  GL.uniform location $= d

getBlockProgram :: Map String GL.Program -> GL.Program
getBlockProgram programs = programs Map.! block

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = either error id <$> readTexture f

buildTextures :: IO (Map String GL.TextureObject)
buildTextures = Map.fromList <$> mapM buildTexture ["block", "paddle"]

buildTexture :: String -> IO (String, GL.TextureObject)
buildTexture name = do
  tx <- loadTex (name ++ ".png")
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  return (name, tx)

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix mat =
  GL.withNewMatrix GL.RowMajor $ \glPtr ->
    zipWithM_
      (pokeElemOff glPtr)
      [0 ..]
      (concat $ Foldable.toList <$> Foldable.toList mat)
