module Main where

import           Control.Monad.State.Strict

import           Game
import           State

main :: IO ()
main = do
  let state = GameState {
    }
  evalStateT game state
