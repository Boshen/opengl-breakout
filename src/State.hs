module State where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

data GameState = GameState
  { gamePrograms  :: Map String GL.Program
  , gameBlocks    :: Map (V3 Float) Block
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
  deriving (Show)

data Action
  = NoOp
  | QuitProgram
  | Move Float
  | StartGame
  deriving (Show, Eq)

data Block = Block
  { blockPos   :: V3 Float
  , blockLevel :: Int
  , blockModel :: M44 Float
  } deriving (Show)

data Paddle = Paddle
  { paddlePos   :: Float
  , paddleModel :: M44 Float
  } deriving (Show)

data Mesh = Mesh
  { meshVAO    :: GL.VertexArrayObject
  , meshVBO    :: GL.BufferObject
  , meshLength :: GL.NumArrayIndices
  } deriving (Show)

data Ball = Ball
  { ballPos      :: V3 Float
  , ballVelocity :: V3 Float
  , ballModel    :: M44 Float
  } deriving (Show)
