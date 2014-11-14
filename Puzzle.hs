{-# LANGUAGE TypeOperators, DataKinds, FlexibleInstances, StandaloneDeriving, FlexibleContexts, KindSignatures #-}
module Puzzle where

import Data.HList
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Reader

newtype Candidate l = Candidate [HList l]

deriving instance Show (Candidate '[])
deriving instance (Show e, Show (HList l)) => Show (Candidate (e ': l))

queryC :: (HOccurs t (HList l), HOccurs r (HList l), Eq t)
      => Candidate l -> t -> Maybe r
queryC (Candidate ls) t = case filter ((==t) . hOccurs) ls of
  (x:_) -> Just $ hOccurs x
  _ -> Nothing


type Test l = ReaderT (Candidate l) Maybe


query :: (HOccurs t (HList l), Eq t, HOccurs r (HList l)) => t -> Test l r
query t = do
  c <- ask
  case queryC c t of
    Just r -> return r
    _ -> mzero

is' :: (r -> Bool) -> Test l r -> Test l ()
is' f test = do
  r <- test
  if f r then return () else mzero

is :: (HOccurs t (HList l), Eq t, HOccurs r (HList l))
   => (r -> Bool) -> t -> Test l ()
is f t = is' f (query t)
  
is2 :: (HOccurs t1 (HList l), HOccurs t2 (HList l), Eq t1, Eq t2, HOccurs r (HList l))
    => (r -> r -> Bool) -> t1 -> t2 -> Test l ()
is2 f t1 t2 = is' (uncurry f) $ liftA2 (,) (query t1) (query t2)

class Generate l where
  generate :: [Candidate l]
  generatePerm :: [Candidate l]

instance Generate '[] where
  generate = [Candidate $ repeat HNil]
  generatePerm = generate

instance (Bounded e, Enum e, Generate l) => Generate (e ': l) where
  generate = map (\(Candidate ls) -> f ls) generatePerm where
    p = [minBound .. maxBound]
    f ls = Candidate $ zipWith HCons p ls
  generatePerm = concatMap (\(Candidate ls) -> f ls) generatePerm where
    ps = permutations [minBound .. maxBound]
    f ls = [Candidate $ zipWith HCons p ls | p <- ps]


solutions :: Generate l => Test l () -> [Candidate l]
solutions test = filter (isJust . runReaderT test) generate
