module State where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

data GameState = GameState
  { gamePrograms :: Map String GL.Program
  , gameBlocks  :: Map (V3 Int) Block
  , gameDimension :: (Int, Int)
  } deriving (Show)

type Game = StateT GameState IO

data Action
  = NoOp
  | QuitProgram
  deriving (Show, Eq)

data Block = Block
  { blockPos   :: V3 Int
  , blockLevel :: Int
  , blockVAO   :: Maybe GL.VertexArrayObject
  , blockVBO   :: Maybe GL.BufferObject
  , blockDirty :: Bool
  , blockModel  :: M44 Float
  } deriving (Show)
