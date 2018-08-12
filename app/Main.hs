module Main where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map

import           Game
import           State

main :: IO ()
main = do
  let state = GameState { gamePrograms = Map.empty
                        , gameBlocks = Map.empty
                        , gameDimension = (800, 600)
                        , gameTextures = Map.empty
                        , gameMeshes = Map.empty
                        }
  State.evalStateT game state
