{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Puzzle.Core where

import Prelude hiding (any, all, (&&))

import Ersatz
import Data.HList
import Data.List hiding (any, all)
import GHC.Generics
import Control.Applicative
import Control.Monad.State
import Control.Arrow ((&&&))

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

type Elem e l = (Enum e, Bounded e, HOccurs (E e) (HList (EList l)))

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


connect :: (Elem a l, Elem b l) => Assignment l -> (E a -> E b -> Bit) -> Bit
connect (Assignment l) f = all (uncurry f) $ map (hOccurs &&& hOccurs) l
