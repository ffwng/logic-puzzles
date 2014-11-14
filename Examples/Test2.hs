{-# LANGUAGE DataKinds #-}
module Main where

import Puzzle

import Control.Monad

data Price = P490 | P535 | P580 | P625
           deriving (Show, Eq, Enum, Bounded, Ord)

data Meal = Cheeseburger | Sandwich | Italian | Meatball
          deriving (Show, Eq, Enum, Bounded)

data Name = Geoffrey | Irene | Peter | Rebecca
          deriving (Show, Eq, Enum, Bounded)

eith a b t = do
  a' <- query t
  b' <- query t
  guard $ a == a' || b == b'

test :: Test '[Price, Meal, Name] ()
test = do
  eith P490 Peter Meatball
  eith Sandwich Geoffrey P535
  is2 ((<) :: Price -> Price -> Bool) Rebecca Sandwich
  is2 (\a b -> fromEnum (a :: Price) == fromEnum b - 1) Peter Italian
  is2 (\a b -> (a,b) `elem` [(Cheeseburger, Sandwich), (Sandwich, Cheeseburger)]) Irene P490
