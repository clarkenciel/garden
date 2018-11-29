module Shoot where

import Data.Text (Text)
import qualified Data.Text as T

import DNA (DNA, clone, splice, occurrencesOf)

import Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Rand
import Control.Monad (replicateM)

data Shoot = Shoot
  { shootSeed :: Char
  , shootDna :: DNA
  }
  deriving (Show)

bud :: MonadRandom m => Int -> Shoot -> m [Shoot]
bud n shoot = replicateM n sprout
  where sprout = clone dna (shootSeed shoot) >>= return . flip Shoot dna
        dna = shootDna shoot

graft :: MonadRandom m => Shoot -> Shoot -> m Shoot
graft s1 s2 = flip Shoot dna <$> seed
  where dna = splice dna1 dna2
        dna1 = shootDna s1
        dna2 = shootDna s2
        seed = let seed1 = shootSeed s1
                   seed2 = shootSeed s2
                   seed1Occurrences = occurrencesOf seed1 dna1 + occurrencesOf seed1 dna2
                   seed2Occurrences = occurrencesOf seed2 dna2 + occurrencesOf seed2 dna2
               in if seed1Occurrences == seed2Occurrences
                  then coinToss seed1 seed2
                  else if seed1Occurrences > seed2Occurrences
                       then return seed1
                       else return seed2
        coinToss x y = do
          b <- Rand.getRandom
          if b then return x else return y
