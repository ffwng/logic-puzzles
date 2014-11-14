{-# LANGUAGE QuasiQuotes, DataKinds #-}
module Main where

-- http://www.onlinewahn.de/hammer-r.htm
-- 0.2% of population… LOL

import Puzzle2
import QQ

[puzzle|
  Farbe = Rot | Silber | Blau | Braun | Gruen
  Auto = Ferrari | VW | BMW | Smart | Ford
  Beruf = Lehrer | Metzger | Notar | Schreiner | Baecker
  Stadt = Muenchen | Hamburg | Koeln | Berlin | Stuttgart
  Musik = Madonna | Abba | Beatles | Eminem | Heino
  Pos = P 1..5
|]

neben :: Pos -> Pos -> Bool
neben (P i) (P j) = abs (i-j) == 1

is' a b = is (==a) b

test :: Test '[Pos, Farbe, Auto, Beruf, Stadt, Musik] IO ()
test = do
  is' Rot Ferrari
  is' Silber Lehrer
  is' VW Madonna
  is' BMW Muenchen
  is2 neben BMW Blau
  is2 neben Hamburg Braun
  is' Abba Metzger
  is2 neben Beatles Lehrer
  is' Notar Koeln
  is2 neben Blau Smart
  is' Ford Schreiner
  is' Gruen Hamburg
  is2 neben Berlin Baecker
  is' (P 4) Eminem
  is2 (\a b -> not $ neben a b) Stuttgart BMW
  
