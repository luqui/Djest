{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes #-}

module Djest.IterativeDeepening where

import Control.Monad.Logic
import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Djest.MonadDelay

newtype T w a = T { runT :: ReaderT w (StateT w Logic) a }
  deriving (Functor, Applicative, Monad, MonadPlus)

instance (Ord w, Num w) => MonadDelay (T w) where
    delay t = T $ do
      s <- lift get
      limit <- ask
      if s < limit
        then lift (put $! s+1) >> runT t
        else mzero

flatten :: (Num w, Ord w) => T w a -> [a]
flatten t = concat [ observeAll (slice t n) | n <- iterate (+1) 0 ]
  where
  slice t n = do
    (x,w) <- runStateT (runReaderT (runT t) n) 0
    guard (w == n)
    return x
