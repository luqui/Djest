{-# LANGUAGE DeriveFunctor, RankNTypes #-}

module Djest.Delay (Delay(..), MonadDelay(..), flattenDelay) where

import Control.Monad.Logic
import Data.Monoid
import Control.Monad
import Control.Applicative
import Djest.MonadDelay

newtype Delay a = Delay { getDelay :: [[a]] }
    deriving (Show, Functor)

flattenDelay :: Delay a -> [a]
flattenDelay (Delay xs) = concat xs

instance Applicative Delay where
    pure = return
    (<*>) = ap

instance Monad Delay where
    return x = Delay [[x]]
    Delay [] >>= f = Delay []
    Delay ([]:xss) >>= f = delay (Delay xss >>= f)
    Delay ((x:xs):xss) >>= f = Delay $ mzip (getDelay (f x)) (getDelay (Delay (xs:xss) >>= f))

instance MonadPlus Delay where
    mzero = Delay []
    Delay a `mplus` Delay b = Delay $ mzip a b

instance MonadDelay Delay where
    delay (Delay xss) = Delay ([]:xss)


mzip :: (Monoid m) => [m] -> [m] -> [m]
mzip xs ys = (x' `mappend` y') : xs' ys'
    where
    (x',xs') = case xs of
                    [] -> (mempty, id)
                    (x:xs) -> (x, mzip xs)
    (y',ys') = case ys of
                    [] -> (mempty, [])
                    (y:ys) -> (y, ys)
