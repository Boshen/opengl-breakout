{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game where

import           Control.Monad.State.Strict
import           Data.Maybe
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear
import           SDL                        (($=))
import qualified SDL
import           SDL.Video.OpenGL           (Mode (Normal))

import           Ball
import           Block
import           Event
import           Mesh
import           Paddle
import           Program
import           State

game :: Game ()
game = do
  window <- create
  play window
  destroy window

create :: Game SDL.Window
create = do
  GameState{..} <- get
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear

  let
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
  SDL.glCreateContext window

  GL.viewport $=
    ( GL.Position 0 0
    , GL.Size (round sw) (round sh)
    )

  gameState <- get
  programs <- liftIO buildPrograms
  textures <- liftIO buildTextures
  meshes <- liftIO buildMeshes
  put $ gameState { gamePrograms = programs
                  , gameTextures = textures
                  , gameMeshes = meshes
                  }
  makeBlocks
  makePaddle 0
  makeBall (V2 40 (sh - 40)) (V2 0 0)
  return window

destroy :: SDL.Window -> Game ()
destroy window = do
  SDL.destroyWindow window
  SDL.quit

play :: SDL.Window -> Game ()
play window = SDL.time >>= loop window

loop :: SDL.Window -> Float -> Game ()
loop window lastFrame = do
  gameState@GameState{..} <- get

  -- get actions from user
  actions <- liftIO $ parseEvents <$> SDL.pollEvents

  currentFrame <- SDL.time
  let dt = currentFrame - lastFrame

  mapM_ updatePaddle actions
  makeBlockCollison
  makePaddleCollison
  checkBallHitBottom
  updateBall dt

  renderBlocks
  renderPaddle
  renderBall

  when (StartGame `elem` actions && gameStatus == GameStopped) $
    put $ gameState { gameStatus = GameStarted
                    , gameBall = Just $ (fromJust gameBall) { ballVelocity = ballInitialVelocity }
                    }

  -- clear frame
  liftIO $ do
    SDL.glSwapWindow window
    GL.clearColor $= GL.Color4 0.1 0.1 0.1 1
    -- GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.clear [GL.ColorBuffer]

  unless (QuitProgram `elem` actions) (loop window currentFrame)
