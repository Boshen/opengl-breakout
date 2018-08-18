module Program where

import           Control.Monad
import qualified Data.Foldable             as Foldable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import           Linear

import           JuicyTextures
import           LoadShaders
import           Textures

block :: String
block = "block"

particle :: String
particle = "particle"

programs :: [(String, String, String, [String])]
programs =
  [ ( block
    , "./shaders/block.vert"
    , "./shaders/block.frag"
    , ["model", "projection", "image", "blockColor"])
  , ( particle
    , "./shaders/particle.vert"
    , "./shaders/particle.frag"
    , ["offset", "color"])
  ]

getUniforms :: [String] -> [(String, GL.AttribLocation)]
getUniforms names =
  map (\(i, name) -> (name, GL.AttribLocation i)) (zip [0 ..] names)

buildPrograms :: IO (Map String GL.Program)
buildPrograms = Map.fromList <$> mapM buildProgram programs

buildProgram :: (String, String, String, [String]) -> IO (String, GL.Program)
buildProgram (name, vert, frag, uniforms) = do
  program <-
    loadShaders
      [ ShaderInfo GL.VertexShader (FileSource vert)
      , ShaderInfo GL.FragmentShader (FileSource frag)
      ]
  mapM_ (\(nm, loc) -> GL.attribLocation program nm $= loc) (getUniforms uniforms)
  return (name, program)

setUniform :: GL.Uniform a => GL.Program -> String -> a -> IO ()
setUniform program name d = do
  location <- GL.uniformLocation program name
  GL.uniform location $= d

getBlockProgram :: Map String GL.Program -> GL.Program
getBlockProgram pgrms = pgrms Map.! block

loadTex :: FilePath -> IO GL.TextureObject
loadTex f = either error id <$> readTexture f

buildTextures :: IO (Map String GL.TextureObject)
buildTextures =
  Map.fromList <$> mapM buildTexture ["block", "paddle", "awesomeface", "particle"]

buildTexture :: String -> IO (String, GL.TextureObject)
buildTexture name = do
  tx <- loadTex (name ++ ".png")

  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  return (name, tx)

toGlMatrix :: M44 Float -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrix = GL.newMatrix GL.RowMajor . (Foldable.toList >=> Foldable.toList)
