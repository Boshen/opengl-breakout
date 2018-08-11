module State where

import           Control.Monad.State.Strict

data GameState = GameState
  { d :: String
  } deriving (Show)

type Game = StateT GameState IO

data Action
  = NoOp
  | QuitProgram
  deriving (Show, Eq)
