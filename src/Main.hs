{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.UUID (UUID)
import qualified Data.UUID.V4 as ID

import qualified Data.Maybe as Maybe

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Data.Set as Set

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.List as L

import           Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Rand

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import System.Console.ANSI (clearScreen)

main :: IO ()
main = do
  let t1 = T.pack "hello, world"
      t2 = T.pack "goodbye, world"
      t3 = T.pack "oogedy boogedy"
  putStr "using plants from: "
  TIO.putStrLn $ T.intercalate (T.pack ", ") [t1, t2, t3]

  let (w, h) = (30, 30)
  points <- Rand.replicateM 3 $ randomPoint (0, w) (0, h)
  plants <- mapM (uncurry sproutAt) (zip [t1, t2, t3] points)

  loop 30 (Garden plants) (C w h)


loop count garden canvas@(C w h) = do
  clearScreen
  TIO.putStrLn $ T.replicate w (T.pack "-")
  render canvas (diagramPlants (plants garden))
  garden' <- grow garden
  threadDelay $ 10000
  when (count > 1) $ loop (pred count) garden' canvas

data Canvas = C Int Int
  deriving (Show)

data Sigil = Sigil Char Point
  deriving (Show, Eq)

instance Ord Sigil where
  compare (Sigil c p) (Sigil c' p') = case compare p p' of
    EQ -> compare c c'
    ordering -> ordering

render canvas sigils = TIO.putStrLn . T.unlines . map T.pack $ draw canvas sigils

positions (C width height) = [[P x y | x <- [0..width-1]] | y <- [0..height-1]]

draw canvas sigils = map lookup <$> (positions canvas)
  where lookup position = maybe ' ' id $ index Map.!? position
        index = L.foldl' insert Map.empty sigils
        insert m (Sigil c p) = Map.insert p c m

diagramPlants plants = concatMap diagramPlant plants
  where diagramPlant Plant{shoots} = map diagramShoot shoots
        diagramShoot Shoot{seed, point} = Sigil seed point

data Garden = Garden
  { plants :: [Plant]
  }
  deriving (Show)

grow :: MonadRandom m => Garden -> m Garden
grow g@Garden{plants} = do
  plants' <- mapM cultivate plants
  let index = L.foldl' (\m p -> Map.insert (plantId p) p m) Map.empty plants'
      grafts = overlaps plants'
  garden <- mapM (trim . L.foldl1' graft . L.map (index Map.!) . Set.toList) grafts
  return $ g { plants = garden }

-- | the positions in a garden where a plant has shoots
plantPositions Plant{plantId,shoots} = L.foldl' index Map.empty shoots
  where index m Shoot{point} = Map.alter (maybe initialize leave) point m
        initialize = Just $ Set.fromList [plantId]
        leave = Just . id

-- | sets of plant ids corresponding to plants that have overlapping shoots
overlaps = plantClusters . pointIntersections
  where pointIntersections = Map.elems . L.foldl1' (Map.unionWith Set.union) . map plantPositions
        plantClusters [] = []
        plantClusters (x:xs) =
          let x' = L.foldl' (\x x' -> if Set.intersection x x' == Set.empty then x else Set.union x x') x xs
          in x' : (plantClusters $ L.filter ((== Set.empty) . Set.intersection x') xs)

data Plant = Plant
  { plantId :: UUID
  , dna :: DNA
  , shoots :: [Shoot]
  }
  deriving (Show)

sproutAt t point = ID.nextRandom >>= \id -> return $ Plant id (toDNA t) [Shoot (T.head t) point]

graft p1 p2 = Plant (plantId p1) dna' shoots'
  where shoots' = concatMap shoots [p1, p2]
        dna' = splice (dna p1) (dna p2)

growths :: MonadRandom m => Plant -> m [Shoot]
growths Plant{dna, shoots} = concat <$> mapM (sprout dna) shoots

cultivate plant = do
  shoots' <- growths plant
  maybe plant (attach plant) <$> choose shoots'

attach plant shoot = plant { shoots = shoot : (shoots plant) }

-- | collapse overlapping shoots within a plant into a single shoot
-- by selecting one shoot for each pair of overlapping shoots
trim :: MonadRandom m => Plant -> m Plant
trim p@Plant{shoots} = do
  shoots' <- map Maybe.fromJust . L.filter Maybe.isJust <$> mapM choose overlaps
  return p { shoots = shoots' }
  where overlaps = map Set.toList $ Map.elems $ L.foldl' upsert Map.empty shoots
        upsert m shoot = Map.alter (maybe (initial shoot) (add shoot)) (point shoot) m
        initial shoot = Just $ Set.fromList [shoot]
        add shoot = Just . Set.insert shoot

data Shoot = Shoot
  { seed :: Char
  , point :: Point
  }
  deriving (Show, Eq, Ord)

sprout :: MonadRandom m => DNA -> Shoot -> m [Shoot]
sprout dna Shoot{seed, point} = mapM ((<$> clone dna seed) . freshShoot) (neighbors point)
  where freshShoot = flip Shoot

data Point = P Int Int
  deriving (Show, Eq, Ord)

neighbors (P x y) =
  [ P (x - 1) y
  , P x (y - 1)
  , P (x + 1) y
  , P x (y + 1)
  ]

randomPoint xRange yRange = P <$> Rand.getRandomR xRange <*> Rand.getRandomR yRange

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
