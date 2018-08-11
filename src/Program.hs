module Program where

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import qualified Graphics.Rendering.OpenGL as GL
import           SDL                       (($=))

import           JuicyTextures
import           Textures
import           LoadShaders

sprite = "sprite"

programs :: [(String, String, String)]
programs = [(sprite, "./shaders/sprite.vert", "./shaders/sprite.frag")]

uniforms :: [(String, GL.AttribLocation)]
uniforms = map
  (\(i, name) -> (name, GL.AttribLocation i))
  (zip [0..] ["model", "projection", "image", "spriteColor"])

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

getSpriteProgram :: Map String GL.Program -> GL.Program
getSpriteProgram programs = programs Map.! sprite

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = either error id <$> readTexture f
