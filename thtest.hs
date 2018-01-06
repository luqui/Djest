{-# LANGUAGE TemplateHaskell, RankNTypes #-}

import Djest.Search

$(define "foo" [t| forall a. a -> a |])
