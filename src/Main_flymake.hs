{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.List as L

import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Rand

main :: IO ()
main = do
  putStrLn "hello world"

data Canvas = C Int Int
  deriving (Show)

data Sigil = Sigil Char Point
  deriving (Show, Eq)

instance Ord Sigil where
  compare (Sigil c p) (Sigil c' p') = case compare p p' of
    EQ -> compare c c'
    ordering -> ordering

positions (C width height) = [[P x y | x <- [0..width-1]] | y <- [0..height-1]]

draw canvas sigils = map lookup <$> (positions canvas)
  where lookup position = maybe ' ' id $ index Map.!? position
        index = L.foldl' insert Map.empty sigils
        insert m (Sigil c p) = Map.insert p c m

data Garden = Garden
  { plants :: [Plant]
  }
  deriving (Show)

grow :: MonadRandom m => Garden -> m Garden
grow g@Garden{plants} = do
  plants' <- mapM cultivate plants
  let (fine, overgrowths) = inspect plants'
      grafted = L.foldl1' graft <$> overgrowths
  return $ g { plants = fine ++ grafted }
    where inspect :: [Plant] -> ([Plant], [[Plant]])
          inspect plants = L.foldl' separate ([], []) plants
          separate (fine, overgrowths) p =

data Plant = Plant
  { dna :: DNA
  , shoots :: [Shoot]
  }
  deriving (Show)

graft p1 p2 = Plant dna' shoots'
  where shoots' = concatMap shoots [p1, p2]
        dna' = splice (dna p1) (dna p2)

growths :: MonadRandom m => Plant -> m [Shoot]
growths Plant{dna, shoots} = concat <$> mapM (sprout dna) shoots

cultivate plant = do
  shoots' <- growths plant
  maybe plant attach <$> choose shoots'
    where attach shoot = plant { shoots = shoot : (shoots plant) }

data Shoot = Shoot
  { seed :: Char
  , point :: Point
  }
  deriving (Show, Eq)

sprout :: MonadRandom m => DNA -> Shoot -> m [Shoot]
sprout dna Shoot{seed, point} =
  mapM ((<$> clone dna seed) . flip Shoot) (neighbors point)

data Point = P Int Int
  deriving (Show, Eq, Ord)

neighbors (P x y) =
  [P x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], abs x' /= abs y']

-- | Right now this will cause my algo to miss out on characters
-- that sort lower lexicographically, but have the same weight.
-- I should look at a better structure, maybe
-- Map Char (Map Float [Char]) and then select randomly from that [Char]
type TransitionTable = Map Char (Map Float [Char])

type TransitionCountTable = Map Char (Map Char Int)

data DNA = DNA
  { raw :: Text
  , transitions :: TransitionTable
  }
  deriving (Show)

toDNA :: Text -> DNA
toDNA t = DNA t (tabulateTransitions $ countTransitions t)

tabulateTransitions :: TransitionCountTable -> TransitionTable
tabulateTransitions counts = Map.map normalizeRow counts
  where normalizeRow :: Map Char Int -> Map Float [Char]
        normalizeRow row =
          let (total, row') =
                Map.foldlWithKey'
                (\(tot, r) k x -> (tot + fromIntegral x, (fromIntegral x, k):r))
                (0.0, [])
                row
          in L.foldl' (\m (count, char) -> Map.alter (maybe (Just [char]) (Just . (char:))) (count / total) m)
             Map.empty
             row'

countTransitions :: Text -> TransitionCountTable
countTransitions t =
  fst $ T.foldl' logTransition (Map.empty, T.head t) (T.tail t)
  where logTransition (transitions, prevChar) char = (updateLog transitions prevChar char, char)
        updateLog transitions prevChar char =
          Map.alter (maybe (Just $ Map.fromList [(char, 1)]) (Just . Map.alter (Just . maybe 1 succ) char))
          prevChar transitions

splice dna1 dna2 = toDNA $ T.append (raw dna1) (raw dna2)

-- | a kind of roulette wheel selection from a markove transition table
-- wherein a "ball" "bounces" across the "slots" of the transition row
-- until it comes to rest in a slot (or reaches the end of the row)
clone :: MonadRandom m => DNA -> Char -> m Char
clone DNA{transitions} seed =
  maybe (return seed) selection $ transitions Map.!? seed
  where selection possibilities = do
          ball <- Rand.getRandomR (0.0 :: Float, 1.0)
          possibilities' <- maybe [] id <$> (bounce ball $ Map.toList possibilities)
          maybe seed id <$> choose possibilities'

choose :: MonadRandom m => [a] -> m (Maybe a)
choose [] = return Nothing
choose xs = do
  ball <- getWeight
  randomlyWeight xs >>= bounce ball

bounce :: MonadRandom m => Float -> [(Float, a)] -> m (Maybe a)
bounce _ [] = return Nothing
bounce velocity (slot@(gravity, choice):slots)
  | velocity <= 0.0 = return $ Just choice
  | otherwise = bounce (velocity - gravity) (slots ++ [slot])

randomlyWeight :: MonadRandom m => [a] -> m [(Float, a)]
randomlyWeight xs = mapM weight xs
  where weight x = flip (,) x <$> getWeight

getWeight :: MonadRandom m => m Float
getWeight = Rand.getRandomR (0.0 :: Float, 1.0)
