{-# LANGUAGE TemplateHaskell, RankNTypes, ScopedTypeVariables, KindSignatures, DeriveFunctor #-}

import Djest.Search

data Tree a
    = L a
    | B (Tree a) (Tree a)
    deriving (Show, Eq)

cata :: (b -> b -> b) -> (a -> b) -> Tree a -> b
cata _ s (L x) = s x
cata b s (B l r) = b (cata b s l) (cata b s r)

deduce "mapTree" [t| forall a b. (a -> b) -> Tree a -> Tree b |]
    [ 'cata, 'L, 'B ]
    [| and [ mapTree id (L 0) == L 0
           , mapTree id (B (L 1) (L 2)) == B (L 1) (L 2)
           ] |]


cat :: String -> String -> String
cat = (++)

deduce "preorder" [t| forall a. (a -> String) -> Tree a -> String |] 
    ['cata, 'cat, 'mapTree]
    [| and [ preorder show (L 1) == "1"
           , preorder show (B (B (L 1) (L 2)) (L 3)) == "123" ] |]

