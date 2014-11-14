{-# LANGUAGE DataKinds #-}
module Main where

import Puzzle2

data Haus = Rot | Gruen | Weiß | Gelb | Blau
          deriving (Show, Eq, Enum, Bounded)

data Land = England | Spanien | Ukraine | Norwegen | Japan
          deriving (Show, Eq, Enum, Bounded)

data Tier = Hund | Schnecke | Fuchs | Pferd | Zebra
          deriving (Show, Eq, Enum, Bounded)

data Trank = Kaffee | Tee | Milch | OSaft | Wasser
           deriving (Show, Eq, Enum, Bounded)

data Zigarette = AltemGold | Kools | Chesterfields | LuckyStrike | Parliaments
               deriving (Show, Eq, Enum, Bounded)

data Pos = P1 | P2 | P3 | P4 | P5
         deriving (Show, Eq, Enum, Bounded, Ord)

links :: Pos -> Pos -> Bool
links p1 p2 = fromEnum p1 + 1 == fromEnum p2

rechts :: Pos -> Pos -> Bool
rechts p1 p2 = links p2 p1

neben :: Pos -> Pos -> Bool
neben p1 p2 = links p1 p2 || rechts p1 p2

test :: Test '[Pos, Haus, Land, Tier, Trank, Zigarette] IO ()
test = do
  is (== England) Rot
  is (== Hund) Spanien
  is (== Gruen) Kaffee
  is (== Tee) Ukraine
  is2 rechts Gruen Weiß
  is (== Schnecke) AltemGold
  is (== Gelb) Kools
  is (== P3) Milch
  is (== P1) Norwegen
  is2 neben Chesterfields Fuchs
  is2 neben Kools Pferd
  is (== OSaft) LuckyStrike
  is (== Parliaments) Japan
  is2 neben Norwegen Blau
  
main :: IO ()
main = solve test >>= print
