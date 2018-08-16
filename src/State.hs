module State where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

data GameState = GameState
  { gamePrograms  :: Map String GL.Program
  , gameBlocks    :: Map (V2 Float) Block
  , gamePaddle    :: Maybe Paddle
  , gameDimension :: (Float, Float)
  , gameTextures  :: Map String GL.TextureObject
  , gameMeshes    :: Map String Mesh
  , gameBall      :: Maybe Ball
  , gameStatus    :: GameStatus
  } deriving (Show)

type Game = StateT GameState IO

data GameStatus
  = GameStarted
  | GameStopped
  deriving (Show, Eq)

data Action
  = NoOp
  | QuitProgram
  | Move Float
  | StartGame
  deriving (Show, Eq)

data Block = Block
  { blockPos   :: V2 Float
  , blockLevel :: Int
  , blockSize  :: (Float, Float)
  , blockModel :: M44 Float
  } deriving (Show)

data Paddle = Paddle
  { paddlePos   :: V2 Float
  , paddleSize  :: (Float, Float)
  , paddleModel :: M44 Float
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
  , ballModel    :: M44 Float
  } deriving (Show)

data Direction
  = UP
  | RIGHT
  | DOWN
  | LEFT
  deriving (Eq)
