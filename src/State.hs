module State where

import           Control.Monad.State.Strict
import           Data.Map.Strict            (Map)
import qualified Graphics.Rendering.OpenGL  as GL
import           Linear

data GameState = GameState
  { gamePrograms :: Map String GL.Program
  , gameSprites  :: Map (V3 Int) Sprite
  } deriving (Show)

type Game = StateT GameState IO

data Action
  = NoOp
  | QuitProgram
  deriving (Show, Eq)

data Sprite = Sprite
  { spritePos   :: V3 Int
  , spriteVAO   :: Maybe GL.VertexArrayObject
  , spriteVBO   :: Maybe GL.BufferObject
  , spriteDirty :: Bool
  , spriteModel  :: M44 Float
  } deriving (Show)
