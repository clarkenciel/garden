module DNA where

import Control.Monad.Random (MonadRandom)
import qualified Control.Monad.Random as Rand

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

import Data.Text (Text)
import qualified Data.Text as T

clone :: MonadRandom m => DNA -> Char -> m Char
clone (DNA transitions) seed = maybe (return seed) select possibilities
  where select choices = maybe seed id <$> Rand.weightedMay choices
        possibilities = Map.toList <$> transitions !? seed

splice :: DNA -> DNA -> DNA
splice (DNA d1) (DNA d2) = DNA $ Map.unionWith (Map.unionWith (+)) d1 d2

type TransitionTable = Map Char (Map Char Rational)

data DNA = DNA TransitionTable
  deriving (Show)

toDNA :: Text -> DNA
toDNA = DNA . countTransitions

countTransitions :: Text -> TransitionTable
countTransitions t = fst $ T.foldl' logTransition (Map.empty, firstChar) (T.snoc otherChars firstChar)
  where logTransition (transitions, prevChar) char = (updateLog transitions prevChar char, char)
        updateLog transitions prevChar char =
          Map.alter (maybe (initial char) (upsert char)) prevChar transitions
        initial char = Just $ Map.fromList [(char, 1)]
        upsert char = Just . Map.alter (Just . maybe 1 succ) char
        firstChar = T.head t
        otherChars = T.tail t

occurrencesOf :: Char -> DNA -> Int
occurrencesOf s (DNA tt) = Map.foldl' count 0 tt
  where count n r = Map.foldlWithKey' countOccurrence n r
        countOccurrence n c _ = if c == s then n + 1 else n

instance Semigroup DNA where
  (<>) = splice

instance Monoid DNA where
  mempty = DNA Map.empty
