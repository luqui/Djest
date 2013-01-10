{-# LANGUAGE DeriveFunctor #-}

module Djest.Heap 
    ( MonadDelay(..)
    , Heap, flattenHeap
    )
where

import Djest.MonadDelay
import Control.Applicative
import Control.Monad (ap, MonadPlus(..))

data Heap w a
    = Fail
    | Yield a (Heap w a)
    | Weight w (Heap w a)
    deriving (Show, Functor)

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
