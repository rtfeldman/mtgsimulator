module MTGSimulator where

import qualified Data.Map.Strict as Map

data Mana = Black | Blue | Green | Red | White | Colorless | Hybrid Mana Mana deriving (Show, Eq, Ord)

type ManaCounts = Map.Map Mana Int

emptyManaCounts :: ManaCounts
emptyManaCounts = Map.empty

addToManaCounts :: ManaCounts -> Mana -> ManaCounts
addToManaCounts counts symbol =
  Map.insert symbol (currentCount + 1) counts
  where currentCount = countBySymbol symbol counts

countBySymbol :: Mana -> ManaCounts -> Int
countBySymbol = Map.findWithDefault 0

countSymbols :: [Mana] -> ManaCounts
countSymbols = foldl addToManaCounts emptyManaCounts

castableWith :: [Mana] -> [Mana] -> Bool
castableWith cost available =
  True
  where costCounts      = countSymbols cost
        availableCounts = countSymbols available
