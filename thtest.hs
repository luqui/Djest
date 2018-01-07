{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, KindSignatures #-}

import Djest.Search

iter :: Integer -> (a -> a) -> (a -> a)
iter 0 _ = id
iter n f = f . iter (n-1) f

inc :: Integer -> Integer
inc = (+1)

zero :: Integer
zero = 0

deduce "add" [t| Integer -> Integer -> Integer |] 
    ['iter, 'zero, 'inc]
    [| and [ add 1 1 == 2 
           , add 4 5 == 9 ] |]

