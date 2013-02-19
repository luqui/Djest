{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Djest.Heap 
    ( MonadDelay(..)
    , Heap, flattenHeap
    , Heap', flattenHeap'
    )
where

import Djest.MonadDelay
import Control.Applicative
import Control.Monad (ap, liftM, MonadPlus(..))

data Heap w a
    = Fail
    | Yield a (Heap w a)
    | Weight w (Heap w a)
    deriving (Show, Functor)

newtype Heap' w a = Heap' { getHeap' :: forall r. (a -> Heap w r) -> Heap w r }

instance Monad (Heap' w) where
    return x = Heap' ($ x)
    m >>= f = Heap' $ \r -> getHeap' m (\x -> getHeap' (f x) r)

instance Functor (Heap' w) where
    fmap = liftM

instance Applicative (Heap' w) where
    pure = return
    (<*>) = ap

instance (Weight w) => MonadPlus (Heap' w) where
    mzero = Heap' $ const Fail
    m `mplus` n = Heap' $ \r -> getHeap' m r `mplus` getHeap' n r

instance (UnitWeight w) => MonadDelay (Heap' w) where
    delay m = Heap' $ \r -> Weight unitWeight (getHeap' m r)

class (Ord w) => Weight w where
    difference :: w -> w -> w

class (Weight w) => UnitWeight w where
    unitWeight :: w

instance Weight Int where
    difference = (-)

instance UnitWeight Int where
    unitWeight = 1


instance (Weight w) => Monad (Heap w) where
    return x = Yield x Fail
    Fail >>= _ = Fail
    Yield x m >>= f = f x `mplus` (m >>= f)
    Weight w m >>= f = Weight w (m >>= f)

instance (Weight w) => MonadPlus (Heap w) where
    mzero = Fail
    Fail `mplus` m = m
    Yield x m `mplus` n = Yield x (m `mplus` n)
    Weight w m `mplus` Fail = Weight w m
    Weight w m `mplus` Yield x n = Yield x (Weight w m `mplus` n)
    Weight w m `mplus` Weight w' n
        = case compare w w' of
            LT -> Weight w (m `mplus` Weight (difference w' w) n)
            EQ -> Weight w (m `mplus` n)
            GT -> Weight w' (Weight (difference w w') m `mplus` n)

instance (Weight w) => Applicative (Heap w) where
    pure = return
    (<*>) = ap

instance (UnitWeight w) => MonadDelay (Heap w) where
    delay = Weight unitWeight
    

flattenHeap :: Heap w a -> [a]
flattenHeap Fail = []
flattenHeap (Yield x hs) = x : flattenHeap hs
flattenHeap (Weight w hs) = flattenHeap hs

flattenHeap' :: Heap' w a -> [a]
flattenHeap' h = flattenHeap (getHeap' h (\x -> Yield x Fail))
