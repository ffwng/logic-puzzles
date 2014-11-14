{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Puzzle2 where

import Prelude hiding (any, (&&))

import Ersatz
import Data.HList
import Data.List hiding (any)
import GHC.Generics
import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader

data E e = E Bit3
         deriving (Show, Typeable, Generic)

instance Boolean (E e)
instance Variable (E e)
instance Equatable (E e)

instance Enum e => Decoding (E e) where
  type Decoded (E e) = e
  decode s (E e) = toEnum . fromIntegral <$> decode s e

instance Enum e => Encoding (E e) where
  type Encoded (E e) = e
  encode e = E $ encode (fromIntegral $ fromEnum e)


type family EList (a :: [*]) :: [*] where
  EList '[] = '[]
  EList (e ': l) = E e ': EList l


data Assignment l = Assignment [HList (EList l)]

class Generate l where
  generate :: (MonadState s m, HasSAT s) => m (Assignment l)
  decode' :: Solution -> HList (EList l) -> Maybe (HList l)

instance Generate '[] where
  generate = return $ Assignment (repeat HNil)
  decode' _ _ = return HNil

instance (Enum e, Bounded e, Generate l) => Generate (e ': l) where
  generate = do
    let values = [minBound .. maxBound]
        count = length values
    vars <- replicateM count exists
    assertRange values vars
    assertAllDifferent vars
    Assignment ls :: Assignment l <- generate
    return $ Assignment (zipWith HCons vars ls)

  decode' sol (HCons e l) = liftA2 HCons e' l' where
    e' = decode sol e
    l' = decode' sol l
  decode' _ _ = Nothing

instance Generate l => Decoding (Assignment l) where
  type Decoded (Assignment l) = [HList l]
  decode sol (Assignment ls) = mapM (decode' sol) ls

assertRange :: (MonadState s m, HasSAT s, Encoding a, Equatable a)
               => [Encoded a] -> [a] -> m ()
assertRange xs vs = forM_ xs $ \x -> assert $ any (=== encode x) vs

assertAllDifferent :: (MonadState s m, HasSAT s, Equatable a)
                      => [a] -> m ()
assertAllDifferent vs = forM_ (orderedPairs vs) $ assert . uncurry (/==)

orderedPairs :: [a] -> [(a, a)]
orderedPairs l = do
  a:rest <- tails l
  b <- rest
  return (a, b)


type Test l m = ReaderT (Assignment l) (StateT SAT m)

type Elem e l = (Enum e, Bounded e, HOccurs (E e) (HList (EList l)))

is :: forall l m e r.
      (Monad m, Elem e l, Elem r l)
      => (r -> Bool) -> e -> Test l m ()
is f e = do
  Assignment ls <- ask
  forM_ ls $ \l -> do
    let v = hOccurs l :: E e
        r = hOccurs l :: E r
    assert $ v === encode e ==> satisfies f r

is2 :: forall l m e1 e2 r1 r2.
       (Monad m, Elem e1 l, Elem e2 l, Elem r1 l, Elem r2 l)
        => (r1 -> r2 -> Bool) -> e1 -> e2 -> Test l m ()
is2 f e1 e2 = do
  Assignment ls <- ask
  forM_ (liftA2 (,) ls ls) $ \(l1, l2) -> do
    let v1 = hOccurs l1 :: E e1
        r1 = hOccurs l1 :: E r1
        v2 = hOccurs l2 :: E e2
        r2 = hOccurs l2 :: E r2
    assert $ v1 === encode e1 && v2 === encode e2 ==> satisfies2 f r1 r2

satisfies :: (Enum a, Bounded a) => (a -> Bool) -> E a -> Bit
satisfies f a = any (\l -> a === encode l) $ filter f [minBound .. maxBound]

satisfies2 :: (Enum a, Bounded a, Enum b, Bounded b)
              => (a -> b -> Bool) -> E a -> E b -> Bit
satisfies2 f a1 a2 = any (\(l1,l2) -> a1 === encode l1 && a2 === encode l2)
                     $ filter (uncurry f)
                     $ liftA2 (,) [minBound .. maxBound] [minBound .. maxBound]


solve :: (Generate l, MonadIO m)
         => Test l m () -> m (Maybe [HList l])
solve test = liftM snd $ solveWith minisat $ do
  problem <- generate
  runReaderT (test >> ask) problem
