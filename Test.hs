{-# LANGUAGE DataKinds #-}
module Main where

import Puzzle

data Time = T1230 | T130 | T200 | T430
          deriving (Show, Eq, Enum, Bounded, Ord)

data Name = Alondra | Angie | Chase | Emerson
          deriving (Show, Eq, Enum, Bounded)

data Spot = Mauna | Meteor | Niagara | Yellowstone
          deriving (Show, Eq, Enum, Bounded)

data Cheese = Feta | Gorgonzola | Limburger | Provolone
            deriving (Show, Eq, Enum, Bounded)

earlier :: Time -> Time -> Bool
earlier = (<)

later :: Time -> Time -> Bool
later = (>)

test :: Test '[Time, Name, Spot, Cheese] ()
test = do
  is2 later Emerson Feta
  is2 (\a b -> (a,b) `elem` [(T1230, T130), (T130, T1230)]) Gorgonzola Chase
  is2 earlier Chase Niagara
  is (/= Mauna) Feta
  is (/= Alondra) Mauna
  is (/= Chase) T1230
  is (== Chase) Yellowstone
  is (/= Emerson) Provolone
  is (/= Angie) Niagara
  is (/= Meteor) Limburger
  is (== Niagara) Provolone
  is (/= Limburger) T200
  is2 (\a b -> a == Meteor || b == Meteor) T1230 T430
