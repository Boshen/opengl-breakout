module Main where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict            as Map

import           Game
import           State

main :: IO ()
main = do
  let state = GameState { gamePrograms = Map.empty
                        , gameBlocks = Map.empty
                        , gamePaddle = Nothing
                        , gameBall = Nothing
                        , gameDimension = (800, 600)
                        , gameTextures = Map.empty
                        , gameMeshes = Map.empty
                        , gameStatus = GameStopped
                        }
  State.evalStateT game state
