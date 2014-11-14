{-# LANGUAGE QuasiQuotes, DataKinds #-}
module Main where

-- http://www.onlinewahn.de/hammer-r.htm
-- 0.2% of population… LOL

import Prelude hiding ((&&), not)

import Puzzle
import Puzzle.QQ
import Ersatz.Bit

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

test :: Test '[Pos, Farbe, Auto, Beruf, Stadt, Musik]
test =
  is Rot Ferrari &&
  is Silber Lehrer &&
  is VW Madonna &&
  is BMW Muenchen &&
  is2 neben BMW Blau &&
  is2 neben Hamburg Braun &&
  is Abba Metzger &&
  is2 neben Beatles Lehrer &&
  is Notar Koeln &&
  is2 neben Blau Smart &&
  is Ford Schreiner &&
  is Gruen Hamburg &&
  is2 neben Berlin Baecker &&
  is (P 4) Eminem &&
  not (is2 neben Stuttgart BMW)
  
