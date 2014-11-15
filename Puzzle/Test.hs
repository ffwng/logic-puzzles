{-# LANGUAGE FlexibleContexts, ConstraintKinds, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, GADTs, ScopedTypeVariables, TypeFamilies #-}
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

type Acc l res = Assignment l -> Bit -> (HList res -> Bit) -> Bit

class IsElem l sel res where
  helper :: HList sel -> Acc l res -> Assignment l -> Bit

instance IsElem l '[] '[] where
  helper _ acc as = acc as true (const true)

instance (Elem a l, Elem b l, IsElem l sel res)
         => IsElem l (a ': sel) (b ': res) where
  helper (HCons a sel) acc = helper (sel :: HList sel) (acc' :: Acc l res) where
    acc' as c f = connect as $ \va vb ->
      acc as (va === encode a && c) (func vb)
      where
        func :: E b -> HList (b ': res) -> Bit
        func vb (HCons b res') = vb === encode b && f res'

isElem :: IsElem l sel res => HList sel -> [HList res] -> Test l
isElem sel res = Test $ do
  as <- ask
  let acc _ b f = b ==> any f res
  return $ helper sel acc as


class Func f where
  type Lift f :: [*]
  call :: f -> HList (Lift f) -> Bool

instance Func Bool where
  type Lift Bool = '[]
  call = const

instance Func f => Func (a -> f) where
  type Lift (a -> f) = a ': Lift f
  call f (HCons a l) = call (f a) l
  call _ _ = error "cannot happen"

class Product l where
  prod :: [HList l]

instance Product '[] where
  prod = [HNil]

instance (Enum e, Bounded e, Product l) => Product (e ': l) where
  prod = HCons <$> [minBound..maxBound] <*> prod

isP :: (IsElem l sel res, Func f, res ~ Lift f, Product res)
       => f -> HList sel -> Test l
isP f sel = isElem sel $ filter (call f) prod

-- common cases

is :: (Elem a l, Elem b l) => a -> b -> Test l
is a b = isElem (a .*. HNil) [b .*. HNil]

isNot :: (Elem a l, Elem b l) => a -> b -> Test l
isNot a b = not (is a b)

isIn :: (Elem a l, Elem b l) => a -> [b] -> Test l
isIn a bs = isElem (a .*. HNil) $ map (.*. HNil) bs

is2 :: (Elem a1 l, Elem a2 l, Elem r1 l, Elem r2 l)
       => (r1 -> r2 -> Bool) -> a1 -> a2 -> Test l
is2 f a1 a2 = isP f (a1 .*. a2 .*. HNil)

{-
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
             $ liftA2 (,) [minBound .. maxBound] [minBound .. maxBound]-}

solve :: (Problem l, MonadIO m) => Test l -> m (Maybe [HList l])
solve test = liftM snd $ solveWith minisat $ do
  problem <- generate
  assert $ runReader (runTest test) problem
  return problem
