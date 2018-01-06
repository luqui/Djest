{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, KindSignatures #-}

import Djest.Search

deduce "foo" [t| forall a. a -> a -> a |] 
    ['const]
    [| and [ foo 1 2 == 1
           , foo "x" "y" == "x"
           ] |]

