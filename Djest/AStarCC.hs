{-# LANGUAGE RankNTypes #-}

module Djest.AStarCC where

import Control.Applicative
import Control.Monad (ap, MonadPlus(..))
import Djest.MonadDelay

newtype AStarCC a = AStarCC { runAStarCC :: forall r. (a -> r -> r) -> r -> (r -> r) -> r }

instance Functor AStarCC where
    fmap f a = AStarCC $ \sc fc dc -> runAStarCC a (sc . f) fc dc

instance Monad AStarCC where
    return x = AStarCC $ \sc fc dc -> sc x fc
    m >>= f = AStarCC $ \sc fc dc -> runAStarCC m (\x r -> runAStarCC (f x) sc r dc) fc dc

instance Applicative AStarCC where
    pure = return
    (<*>) = ap

instance MonadPlus AStarCC where
    mzero = AStarCC $ \sc fc dc -> fc
    m1 `mplus` m2 = AStarCC $ \sc fc dc -> 
        runAStarCC m1 sc
                      (runAStarCC m2 sc fc dc)
                      (\k -> runAStarCC m2 sc k dc)

instance MonadDelay AStarCC where
    delay m = AStarCC $ \sc fc dc -> dc (runAStarCC m sc fc dc) 
