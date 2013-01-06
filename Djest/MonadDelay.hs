module Djest.MonadDelay where

import Control.Applicative
import Control.Monad
import Control.Monad.State

class (Applicative m, Monad m, MonadPlus m) => MonadDelay m where
    delay :: m a -> m a

instance (MonadDelay m) => MonadDelay (StateT s m) where
    delay m = StateT $ delay . runStateT m
