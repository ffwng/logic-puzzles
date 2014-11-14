{-# LANGUAGE QuasiQuotes, DataKinds #-}
module Main where

import Puzzle2
import QQ

[puzzle|
 Beruf = Schlosser | Schweisser | Landwirt | Ingenieur
 Farbe = Rot | Blau | Gruen | Gelb
 Auto = Renault | Audi | VW | Honda
 Trank = Schnaps | Milch | Saft | Kakao
 Pos = P 1..4
|]

neben :: Pos -> Pos -> Bool
neben (P a) (P b) = abs (a-b) == 1

notNeben :: Pos -> Pos -> Bool
notNeben a b = not $ neben a b

test :: Test '[Pos, Beruf, Farbe, Auto, Trank] IO ()
test = do
  is2 neben Blau Ingenieur
  is2 neben Audi Milch
  is (== Saft) Renault
  is2 notNeben Schweisser Saft
  is (/= Renault) Gruen
  is (/= Honda) Rot
  is (== Schnaps) Honda
  is2 notNeben Audi Gruen
  is2 neben Audi Gelb
  is2 notNeben VW Schlosser
  is (== Audi) Ingenieur
  is2 notNeben Renault Gelb
  is2 notNeben Rot Schweisser
  is2 neben Rot Schlosser
  is2 notNeben Schweisser Landwirt
  is (/= Blau) Milch
  is (/= Blau) (P 1)
  is (/= Saft) Gelb
  is2 neben VW Schweisser
