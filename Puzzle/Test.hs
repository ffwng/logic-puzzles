{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds, GeneralizedNewtypeDeriving #-}
module Puzzle.Test where

import Prelude hiding (any, all, (&&), (||), not)

import Data.HList hiding (apply)
import Control.Applicative
import Control.Monad.Reader
import Ersatz

import Puzzle.Core

newtype TestM l b = Test { runTest :: Reader (Assignment l) b }
                  deriving (Functor, Applicative, Monad)

type Test l = TestM l Bit

apply :: Assignment l -> TestM l b -> b
apply p t = runReader (runTest t) p

instance Boolean b => Boolean (TestM l b) where
  bool = return . bool
  true = return true
  false = return false
  (&&) = liftM2 (&&)
  (||) = liftM2 (||)
  (==>) = liftM2 (==>)
  not = fmap not

  all f l = Test . reader $ \r -> all (apply r . f) l
  any f l = Test . reader $ \r -> any (apply r . f) l

  xor = liftM2 xor

  choose f t b = Test . reader $ \r -> choose (apply r f) (apply r t) (apply r b)

-- is a b === isP (== a) b
is :: (Elem a l, Elem b l) => a -> b -> Test l
is a b = Test $ do
  as <- ask
  return $ connect as $ \va vb -> va === encode a ==> vb === encode b

-- isNot a b === isP (/= a) b
isNot :: (Elem a l, Elem b l) => a -> b -> Test l
isNot a b = Test $ do
  as <- ask
  return $ connect as $ \va vb -> va === encode a ==> vb /== encode b

isP :: (Elem a l, Elem b l) => (b -> Bool) -> a -> Test l
isP f a = Test $ do
  as <- ask
  return $ connect as
    $ \va vb -> va === encode a ==> any (\v -> vb === encode v) values
  where
    values = filter f [minBound .. maxBound]
  
is2 :: (Elem a1 l, Elem r1 l, Elem a2 l, Elem r2 l)
       => (r1 -> r2 -> Bool) -> a1 -> a2 -> Test l
is2 f a1 a2 = Test $ do
  as <- ask
  return $ connect as $ \va1 vr1 ->
    connect as $ \va2 vr2 ->
      va1 === encode a1 && va2 === encode a2
      ==> any (\(v1,v2) -> vr1 === encode v1 && vr2 === encode v2) values
  where
    values = filter (uncurry f)
             $ liftA2 (,) [minBound .. maxBound] [minBound .. maxBound]

solve :: (Problem l, MonadIO m) => Test l -> m (Maybe [HList l])
solve test = liftM snd $ solveWith minisat $ do
  problem <- generate
  assert $ runReader (runTest test) problem
  return problem
