{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell, KindSignatures #-}

import Djest.Search

iter :: Integer -> (a -> a) -> (a -> a)
iter 0 _ = id
iter n f = f . iter (n-1) f

zero :: Integer
zero = 0

suc :: Integer -> Integer
suc = succ

deduce "addChurch" [t| Integer -> Integer -> Integer |]
    ['iter, 'suc, 'zero]
    [| and [ addChurch 0 0 == 0
           , addChurch 0 1 == 1
           , addChurch 1 0 == 1
           , addChurch 0 2 == 2
           , addChurch 2 0 == 2
           , addChurch 1 1 == 2
           ] |]

deduce "predChurch" [t| Integer -> Integer |]
    ['iter, 'suc, 'zero]
    [| and [ predChurch 1 == 0
           , predChurch 2 == 1
           , predChurch 3 == 2
           ] |]

-- We use a wrapper around List because I'm too lazy to properly
-- handle ListT in template haskell. :-P
newtype List a = List { getList :: [a] }
    deriving (Eq, Show)

foldList :: (a -> b -> b) -> b -> List a -> b
foldList c n (List xs) = foldr c n xs

cons :: a -> List a -> List a
cons x (List xs) = List (x:xs)

nil :: List a
nil = List []


deduce "snoc" [t| forall a. List a -> a -> List a |]
    ['foldList, 'cons, 'nil]
    [| and [ snoc (List []) 1 == List [1]
           , snoc (List [1]) 2 == List [1,2]
           , snoc (List [1,2,3]) 4 == List [1,2,3,4]
           ] |]


-- Deducing reverse is too hard by itself, but if we do snoc first it's easy.
deduce "reverseChurch" [t| forall a. List a -> List a |]
    ['foldList, 'cons, 'nil]
    [| and [ reverseChurch (List []) == List ([] :: [()])
           , reverseChurch (List [1,2,3]) == List [3,2,1]
           ] |]

main = print $ reverseChurch (List [1,2,3,4,5])
