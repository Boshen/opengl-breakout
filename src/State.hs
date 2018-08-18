{-# LANGUAGE NamedFieldPuns #-}

module State where

import           Collidable
import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import           Data.Vector
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

data GameState = GameState
  { gamePrograms  :: Map String GL.Program
  , gameBlocks    :: Map (V2 Float) Block
  , gamePaddle    :: Paddle
  , gameParticles :: Vector Particle
  , gameDimension :: (Float, Float)
  , gameTextures  :: Map String GL.TextureObject
  , gameMeshes    :: Map String Mesh
  , gameBall      :: Ball
  } deriving (Show)

type Game = StateT GameState IO

data Action
  = NoOp
  | QuitProgram
  | Move Float
  | StartGame
  deriving (Show, Eq)

data Block = Block
  { blockPos   :: V2 Float
  , blockSize  :: V2 Float
  , blockLevel :: Int
  } deriving (Show)

data Paddle = Paddle
  { paddlePos  :: V2 Float
  , paddleSize :: V2 Float
  } deriving (Show)

data Mesh = Mesh
  { meshVAO    :: GL.VertexArrayObject
  , meshVBO    :: GL.BufferObject
  , meshLength :: GL.NumArrayIndices
  } deriving (Show)

data Ball = Ball
  { ballPos      :: V2 Float
  , ballRadius   :: Float
  , ballVelocity :: V2 Float
  } deriving (Show)

data Direction
  = UP
  | RIGHT
  | DOWN
  | LEFT
  deriving (Eq)

instance Collidable Ball where
  pos = ballPos
  size Ball { ballRadius } = V2 ballRadius ballRadius

instance Collidable Block where
  pos = blockPos
  size = blockSize

instance Collidable Paddle where
  pos = paddlePos
  size = paddleSize

data Particle = Particle
  { particlePos      :: V2 Float
  , particleVelocity :: V2 Float
  , particleColor    :: V4 Float
  , particleLife     :: Float
  } deriving (Show)
