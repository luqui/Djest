{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables #-}

import Djest.Search

define "foo" [t| forall a. a -> a -> a |] 
    [| and [ foo 1 2 == 1
           , foo "x" "y" == "x"
           ] |]
