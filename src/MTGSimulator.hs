module MTGSimulator where

import qualified Data.Map.Strict as Map

data Mana = Black | Blue | Green | Red | White | Colorless | Hybrid Mana Mana deriving (Show, Eq, Ord)

addToSymbolCount :: Map.Map Mana Int -> Mana -> Map.Map Mana Int
addToSymbolCount counts symbol =
  Map.insert symbol (currentCount + 1) counts
  where currentCount = Map.findWithDefault 0 symbol counts

countSymbols :: [Mana] -> Map.Map Mana Int
countSymbols = foldl addToSymbolCount (Map.empty :: Map.Map Mana Int)

castableWith :: [Mana] -> [Mana] -> Bool
castableWith cost available =
  True
  where costCounts      = countSymbols cost
        availableCounts = countSymbols available
