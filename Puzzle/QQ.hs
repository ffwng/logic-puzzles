{-# LANGUAGE TemplateHaskell #-}
module Puzzle.QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative
import Data.Maybe

quotePuzzleDec :: String -> Q [Dec]
quotePuzzleDec s = do
  let ps = parsePuzzle s
  return $ concatMap toDec ps

puzzle :: QuasiQuoter
puzzle = QuasiQuoter {
  quoteDec = quotePuzzleDec,
  quoteExp = fail "not supported",
  quotePat = fail "not supported",
  quoteType = fail "not supported"
  }


data Choice = List [Name] | Range Name Integer Integer
            deriving (Show, Eq)

data Puzzle = Puzzle Name Choice

toDec :: Puzzle -> [Dec]
toDec (Puzzle n c) = DataD [] n [] constr deriv : inst where
  constr = case c of
    List alt -> map (\p -> NormalC p []) alt
    Range p _ _ -> [NormalC p [(NotStrict, ConT ''Int)]]

  deriv = case c of
    List _ -> [''Show, ''Eq, ''Ord, ''Enum, ''Bounded]
    Range _ _ _ -> [''Show, ''Eq, ''Ord]

  inst = case c of
    List _ -> []
    Range p s e -> [enumI, boundedI] where
      enumI = InstanceD [] (AppT (ConT ''Enum) (ConT n)) [
        FunD 'toEnum [Clause [] (NormalB $ ConE p) []],
        FunD 'fromEnum [Clause [ConP p [VarP pat]] (NormalB $ VarE pat) []]
        ]

      boundedI = InstanceD [] (AppT (ConT ''Bounded) (ConT n)) [
        FunD 'minBound [Clause [] (NormalB $ foo s) []],
        FunD 'maxBound [Clause [] (NormalB $ foo e) []]
        ]

      foo i = AppE (ConE p) (LitE $ IntegerL i)
      
      pat = mkName "x"


parsePuzzle :: String -> [Puzzle]
parsePuzzle s = case mapM (parse puzzleParser "") (lines s) of
  Left e -> fail (show e)
  Right ps -> catMaybes ps


langDef :: LanguageDef st
langDef = haskellStyle {
  identStart = upper,
  reservedOpNames = ["=", "|", ".."]
  }

lexer :: TokenParser st
lexer = makeTokenParser langDef

puzzleParser :: Parser (Maybe Puzzle)
puzzleParser = whiteSpace lexer >> optionMaybe definition

definition :: Parser Puzzle
definition = do
  n <- identifier lexer
  reservedOp lexer "="
  c <- choice_
  return $ Puzzle (mkName n) c

choiceRange :: Parser Choice
choiceRange = do
  n <- identifier lexer
  s <- fromIntegral <$> integer lexer
  reservedOp lexer ".."
  e <- fromIntegral <$> integer lexer
  return $ Range (mkName n) s e

choiceList :: Parser Choice
choiceList = List <$> sepBy1 (mkName <$> identifier lexer) (reservedOp lexer "|")

choice_ :: Parser Choice
choice_ = try choiceRange <|> choiceList
