{-# LANGUAGE RecordWildCards #-}

module Particle where

import           Control.Lens
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Vector                (Vector)
import           Data.Vector                as V
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

import           Program
import           State

makeParticles :: Vector Particle
makeParticles = V.replicate 500 particle
  where
    particle =
      Particle
        { particlePos = V2 0 0
        , particleVelocity = V2 0 0
        , particleColor = V4 1 1 1 1
        , particleLife = 0
        }

updateParticles :: Float -> Float -> Ball -> Vector Particle -> Vector Particle
updateParticles dt rand ball particles =
  let unused = V.take 2 $ V.findIndices (\p -> particleLife p <= 0) particles
      particles' =
        V.update_ particles unused (V.generate 2 (\_ -> respawnParticle rand ball))
   in updateParticle dt <$> particles'

updateParticle :: Float -> Particle -> Particle
updateParticle dt p@Particle {..} =
  let life = particleLife - dt
   in p
        { particleLife = life
        , particlePos =
            if life > 0
              then particlePos - particleVelocity ^* 0.01
              else particlePos
        , particleColor =
            if life > 0
              then particleColor & _w .~ (particleColor ^. _w * 0.1)
              else particleColor
        }

respawnParticle :: Float -> Ball -> Particle
respawnParticle rand Ball {..} =
  let r = (rand - 50) / 100
      c = 0.5 + rand / 100
   in Particle
        { particlePos = ballPos + pure r + (pure ballRadius ^/ 2)
        , particleVelocity = ballVelocity ^* 0.1
        , particleColor = V4 c c c 1
        , particleLife = 1
        }

renderParticles :: Game ()
renderParticles = do
  GameState {..} <- get
  let Mesh {..} = gameMeshes Map.! "particle"
      tex = gameTextures Map.! "particle"
  GL.blendFunc $= (GL.SrcAlpha, GL.One)
  (Just program) <- GL.get GL.currentProgram
  V.forM_ (V.filter (\p -> particleLife p > 0) gameParticles) $ \Particle {..} ->
    liftIO $ do
      glProjectionMatrix <- toGlMatrix $ ortho 0 800 600 0 (-1) 1
      setUniform program "projection" glProjectionMatrix
      setUniform program "offset" (let V2 x y = particlePos in GL.Vertex2 x y)
      setUniform program "color" (let V4 x y z w = particleColor in GL.Vertex4 x y z w)
      GL.textureBinding GL.Texture2D $= Just tex
      GL.bindVertexArrayObject $= Just meshVAO
      GL.drawArrays GL.Triangles 0 meshLength
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
