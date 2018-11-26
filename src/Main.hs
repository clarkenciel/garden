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

import qualified Graphics.Vty as Vty

import Control.Arrow ((&&&))
import Control.Monad (replicateM_, when)
import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

main :: IO ()
main = do
  (countString, words) <- (head &&& tail) <$> getArgs
  let count = read countString :: Int
      ts = T.pack <$> words

  plants <- sequence $ zipWith (\t p -> p >>= sproutAt t) ts (repeat $ randomPoint (0, w) (0, h))

  term <- Vty.standardIOConfig >>= Vty.mkVty
  loop term count (Garden plants) (C w h)
  Vty.shutdown term
  where (w, h) = (100, 60)

collectWords =
  putStrLn "Please enter some words (press enter to finish collection)" >> collect []
  where collect ses = TIO.putStr prompt >> TIO.getLine >>= return . parse >>= maybe (return ses) (collect . (:ses))
        prompt = T.pack " > "
        parse t
          | T.null t = Nothing
          | otherwise = Just t


loop term count garden canvas@(C w h) = do
  render term canvas (diagramPlants (plants garden))
  garden' <- grow garden
  threadDelay $ 50000
  when (count > 1) $ loop term (pred count) garden' canvas

render term canvas@(C w h) sigils = Vty.update term pic
  where pic = Vty.picForLayers strokes
        strokes = toStroke <$> sigils
        toStroke (Sigil c (P x y)) = Vty.translate x y $ Vty.char Vty.defAttr c

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

cultivate plant@Plant{shoots} = do
  shoots' <- filter (not . (`Set.member` Set.fromList shoots)) <$> growths plant
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
sprout dna Shoot{seed, point} = mapM cloneIntoShoot (neighbors point)
  where freshShoot = flip Shoot
        cloneIntoShoot pt = freshShoot pt <$> clone dna seed

clone :: MonadRandom m => DNA -> Char -> m Char
clone DNA{transitions} seed =
  maybe (return seed) selection $ transitions Map.!? seed
  where selection possibilities = maybe seed id <$> Rand.weightedMay possibilities

data Point = P Int Int
  deriving (Show, Eq, Ord)

neighbors (P x y) =
  [ P (x - 1) y
  , P x (y - 1)
  , P (x + 1) y
  , P x (y + 1)
  ]

randomPoint xRange yRange = P <$> Rand.getRandomR xRange <*> Rand.getRandomR yRange

type TransitionTable = Map Char [(Char, Rational)]

type TransitionCountTable = Map Char (Map Char Int)

data DNA = DNA
  { raw :: Text
  , transitions :: TransitionTable
  }
  deriving (Show)

toDNA :: Text -> DNA
toDNA t = DNA t (tabulateTransitions $ countTransitions t)

tabulateTransitions :: TransitionCountTable -> TransitionTable
tabulateTransitions = Map.map normalizeRow
  where normalizeRow :: Map Char Int -> [(Char, Rational)]
        normalizeRow row = map (fmap normalize) row'
          where (total, row') = Map.foldlWithKey' (\(t, r) k x -> (t + x, (k, x):r)) (0, []) row
                normalize count = fromIntegral count / fromIntegral total


countTransitions :: Text -> TransitionCountTable
countTransitions t =
  fst $ T.foldl' logTransition (Map.empty, T.head t) (T.tail t)
  where logTransition (transitions, prevChar) char = (updateLog transitions prevChar char, char)
        updateLog transitions prevChar char =
          Map.alter (maybe (Just $ Map.fromList [(char, 1)]) (Just . Map.alter (Just . maybe 1 succ) char))
          prevChar transitions

splice dna1 dna2 = toDNA $ T.append (raw dna1) (raw dna2)

choose :: MonadRandom m => [a] -> m (Maybe a)
choose = Rand.uniformMay
