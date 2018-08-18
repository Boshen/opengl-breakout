{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import Data.Word
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Graphics.Rendering.OpenGL  (($=))
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import qualified SDL
import           SDL.Video.OpenGL           (Mode (Normal))
import System.Random

import           Ball
import           Block
import           Event
import           Mesh
import           Paddle
import           Particle
import           Program
import           State

game :: IO ()
game = do
  (window, st) <- create
  play window st
  destroy window

create :: IO (SDL.Window, GameState)
create = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  let
    gameDimension = (800, 600)
    (sw, sh) = gameDimension
    openGLConfig = SDL.OpenGLConfig
      { SDL.glColorPrecision = V4 8 8 8 0
      , SDL.glDepthPrecision = 24
      , SDL.glStencilPrecision = 8
      , SDL.glMultisampleSamples = 1
      , SDL.glProfile = SDL.Core Normal 4 1
      }
  window <- SDL.createWindow
    "Breakout"
    SDL.defaultWindow
      { SDL.windowInitialSize = V2 (round sw) (round sh)
      , SDL.windowOpenGL = Just openGLConfig
      }
  SDL.showWindow window
  _ <- SDL.glCreateContext window

  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (round sw) (round sh)
    )
  GL.blend $= GL.Enabled

  prgs <- liftIO buildPrograms
  textures <- liftIO buildTextures
  meshes <- liftIO buildMeshes
  let st = GameState { gamePrograms = prgs
                     , gameBlocks = makeBlocks gameDimension
                     , gamePaddle = makePaddle gameDimension 0
                     , gameBall = makeBall (V2 40 (sh - 40)) (V2 0 0)
                     , gameParticles = makeParticles
                     , gameDimension = gameDimension
                     , gameTextures = textures
                     , gameMeshes = meshes
                     }
  return (window, st)

destroy :: SDL.Window -> IO ()
destroy window = do
  SDL.destroyWindow window
  SDL.quit

play :: SDL.Window -> GameState -> IO ()
play window st = do
  time <- SDL.ticks
  evalStateT (loop window time) st

loop :: SDL.Window -> Word32 -> Game ()
loop window lastFrame = do
  gameState@GameState{..} <- get

  -- get actions from user
  actions <- liftIO $ parseEvents <$> SDL.pollEvents

  currentFrame <- SDL.ticks
  rand <- liftIO (getStdRandom $ randomR (0, 100))
  let dt = fromIntegral (currentFrame - lastFrame) / 1000
      paddle = updatePaddle gameDimension gamePaddle actions
      (ball', blocks) = makeBlockCollison gameBall gameBlocks
      ball = ballHitBottom gameDimension . updateBall gameDimension . makePaddleCollison paddle $ ball'
  put gameState { gamePaddle = paddle
                , gameBall = ball
                , gameBlocks = blocks
                , gameParticles = updateParticles dt rand ball gameParticles
                }

  GL.currentProgram $= Just (gamePrograms Map.! "block")
  renderBlocks
  renderPaddle
  renderBall

  GL.currentProgram $= Just (gamePrograms Map.! "particle")
  renderParticles

  when (StartGame `elem` actions && ballVelocity ball == V2 0 0) $
    put $ gameState { gameBall = gameBall { ballVelocity = ballInitialVelocity }
                    }

  -- clear frame
  liftIO $ do
    SDL.glSwapWindow window
    GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
    -- GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.clear [GL.ColorBuffer]

  unless (QuitProgram `elem` actions) (loop window currentFrame)
