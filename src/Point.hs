module Point where

import qualified Control.Monad.Random as Rand

data Point = P Int Int
  deriving (Show, Eq, Ord)

neighbors (P x y) =
  [ P (x - 1) y
  , P x (y - 1)
  , P (x + 1) y
  , P x (y + 1)
  ]

randomPoint xRange yRange = P <$> Rand.getRandomR xRange <*> Rand.getRandomR yRange
