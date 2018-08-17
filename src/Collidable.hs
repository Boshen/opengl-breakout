module Collidable where

import           Linear (V2)

class Collidable a where
  pos :: a -> V2 Float
  size :: a -> V2 Float
