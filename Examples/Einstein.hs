{-# LANGUAGE DataKinds, QuasiQuotes #-}
module Main where

import Prelude hiding ((&&), (||))

import Puzzle
import Puzzle.QQ
import Ersatz.Bit

[puzzle|
 Haus = Rot | Gruen | Weiß | Gelb | Blau
 Land = England | Spanien | Ukraine | Norwegen | Japan
 Tier = Hund | Schnecke | Fuchs | Pferd | Zebra
 Trank = Kaffee | Tee | Milch | OSaft | Wasser
 Zigarette = AltemGold | Kools | Chesterfields | LuckyStrike | Parliaments
 Pos = P 1..5
|]

links :: Pos -> Pos -> Bool
links (P i) (P j) = j == i + 1

rechts :: Pos -> Pos -> Bool
rechts p1 p2 = links p2 p1

neben :: Pos -> Pos -> Bool
neben p1 p2 = links p1 p2 || rechts p1 p2

test :: Test '[Pos, Haus, Land, Tier, Trank, Zigarette]
test =
  is England Rot &&
  is Hund Spanien &&
  is Gruen Kaffee &&
  is Tee Ukraine &&
  is2 rechts Gruen Weiß &&
  is Schnecke AltemGold &&
  is Gelb Kools &&
  is (P 3) Milch &&
  is (P 1) Norwegen &&
  is2 neben Chesterfields Fuchs &&
  is2 neben Kools Pferd &&
  is OSaft LuckyStrike &&
  is Parliaments Japan &&
  is2 neben Norwegen Blau
  
main :: IO ()
main = solve test >>= print
