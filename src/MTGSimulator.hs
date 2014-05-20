{-# LANGUAGE DataKinds, FlexibleContexts, KindSignatures,
             PolyKinds, TypeFamilies, TypeOperators, GADTs #-}

module MTGSimulator where

import Control.Applicative
import Data.Type.Equality
import Data.Proxy
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
castableWith available cost = all hasAdequateManaFor $ Map.keys costCounts
  where costCounts      = countSymbols cost
        availableCounts = countSymbols available
        hasAdequateManaFor symbol = amountAvailable >= amountNeeded
          where amountAvailable = countBySymbol symbol availableCounts
                amountNeeded    = countBySymbol symbol costCounts

-- here begins the experimentation, note, this is a decidedly unfancy
-- way to solve this problem. How I tend to roll. I can point you in
-- the direction of fancy if you want it.

manaMinimum :: Int
manaMinimum = 0
manaMax :: Int
manaMax = 12

-- you don't export the newtype value constructor (just the type constructor)
-- so users can't bypass the validation. This'll be a nice demo of applicatives anyway.

newtype ManaCost = ManaCost Int deriving (Eq, Show)
mkManaCost :: Int -> Maybe ManaCost
mkManaCost cost
  | cost < manaMinimum = Nothing
  | cost > manaMax     = Nothing
  | otherwise = Just $ ManaCost cost

-- mkManaCost (-1) -> Nothing
-- mkManaCost 0    -> Nothing

data CardManaCost = CardManaCost { blackManaCost      :: ManaCost
                                 , blueManaCost       :: ManaCost
                                 , greenManaCost      :: ManaCost
                                 , redManaCost        :: ManaCost
                                 , whiteManaCost      :: ManaCost
                                 , colorlessManaCost  :: ManaCost } deriving (Eq, Show)

-- example of how to work with datatype

zeroCostCard :: Maybe CardManaCost
zeroCostCard = CardManaCost <$>
               mkManaCost 0 <*>
               mkManaCost 0 <*>
               mkManaCost 0 <*>
               mkManaCost 0 <*>
               mkManaCost 0 <*>
               mkManaCost 0
