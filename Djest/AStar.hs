{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Djest.AStar (AStar(..), MonadDelay(..), flattenAStar) where

import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Djest.MonadDelay

newtype AStar a = AStar { getAStar :: [[a]] }
    deriving (Show, Functor)

flattenAStar :: AStar a -> [a]
flattenAStar (AStar xs) = concat xs

instance Applicative AStar where
    pure = return
    (<*>) = ap

instance Monad AStar where
    return x = AStar [[x]]
    AStar [] >>= f = AStar []
    AStar ([]:xss) >>= f = delay (AStar xss >>= f)
    AStar ((x:xs):xss) >>= f = AStar $ mzip (getAStar (f x)) (getAStar (AStar (xs:xss) >>= f))

instance MonadPlus AStar where
    mzero = AStar []
    AStar a `mplus` AStar b = AStar $ mzip a b

instance MonadDelay AStar where
    delay (AStar xss) = AStar ([]:xss)


mzip :: (Monoid m) => [m] -> [m] -> [m]
mzip xs ys = (x' `mappend` y') : mzip xs' ys'
    where
    (x',xs') = case xs of
                    [] -> (mempty, [])
                    (x:xs) -> (x, xs)
    (y',ys') = case ys of
                    [] -> (mempty, [])
                    (y:ys) -> (y, ys)

