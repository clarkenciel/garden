module Positioned
  ( position
  , generateNeighbors
  ) where

import Point (Point, neighbors)
import Control.Monad.Random (MonadRandom)
import Control.Monad (replicateM)

data Positioned a = Positioned Point a
  deriving (Show)

generateNeighbors :: MonadRandom m => (a -> m b) -> Positioned a -> m [Positioned b]
generateNeighbors gen (Positioned p a) = replicateM (length ns) (gen a) >>= return . zipWith Positioned ns
  where ns = neighbors p

position :: Point -> a -> Positioned a
position = Positioned
