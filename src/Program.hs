module Program where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Graphics.Rendering.OpenGL as GL
import           SDL                       (($=))

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
buildTextures = do
  tx <- loadTex "block.png"
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just tx
  return $ Map.fromList [("block", tx)]
