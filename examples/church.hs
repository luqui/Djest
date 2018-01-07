{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

import Djest.Search

newtype Church = Church { getChurch :: forall r. (r -> r) -> r -> r }

toChurch :: Integer -> Church
toChurch 0 = Church $ \f x -> x
toChurch n = Church $ \f x -> f (getChurch (toChurch (n-1)) f x)

fromChurch :: Church -> Integer
fromChurch ch = getChurch ch succ (0 :: Integer)

addChurch :: Integer -> Integer -> Integer
addChurch = adapt f
    where
    f = search "(forall r. (r -> r) -> r -> r) -> (forall r. (r -> r) -> r -> r) -> (forall r. (r -> r) -> r -> r)" $
                \(f :: Church -> Church -> Church) ->
                    let f' = adapt f in
                    and [
                        f' 0 0 == 0,
                        f' 0 1 == 1,
                        f' 1 0 == 1,
                        f' 0 2 == 2,
                        f' 2 0 == 2,
                        f' 1 1 == 2
                    ]
    adapt :: (Church -> Church -> Church) -> Integer -> Integer -> Integer
    adapt f x y = fromChurch (f (toChurch x) (toChurch y))

predChurch :: Integer -> Integer
predChurch = adapt f
    where
    f = search "(forall r. (r -> r) -> r -> r) -> (forall r. (r -> r) -> r -> r)" $
                \(f :: Church -> Church) ->
                    let f' = adapt f in
                    and [
                        f' 1 == 0,
                        f' 2 == 1,
                        f' 3 == 2
                    ]
    adapt :: (Church -> Church) -> Integer -> Integer
    adapt f x = fromChurch (f (toChurch x))


-- It can do reverse too, though it takes a lot of work to convince the
-- typechecker that we're legit.

newtype ChurchList a = ChurchList { getChurchList :: forall r. (a -> r -> r) -> r -> r }

fromChurchList :: ChurchList a -> [a]
fromChurchList l = getChurchList l (:) []

toChurchList :: [a] -> ChurchList a
toChurchList xs = ChurchList (\f z -> foldr f z xs)

newtype ChurchListFunc = ChurchListFunc { getChurchListFunc :: forall a. ChurchList a -> ChurchList a }

newtype ChurchListSnocFunc = ChurchListSnocFunc { getChurchListSnocFunc :: forall a. (ChurchList a -> a -> ChurchList a) -> ChurchList a -> ChurchList a }

-- Deducing reverse is too hard to be done quickly, but if snoc is assumed
-- then it is found immediately.
reverseChurch :: [a] -> [a]
reverseChurch = adapt f
  where
    f :: ChurchListSnocFunc
    f = search "forall a. ((forall r. (a -> r -> r) -> r -> r) -> a -> (forall r. (a -> r -> r) -> r -> r)) -> (forall r. (a -> r -> r) -> r -> r) -> (forall r. (a -> r -> r) -> r -> r)" $
               \(f :: ChurchListSnocFunc) ->
                  let f' = adapt f in
                  and [
                      f' [] == ([] :: [()]),
                      f' [1,2,3] == [3,2,1]
                  ]
    adapt :: ChurchListSnocFunc -> [a] -> [a]
    adapt f x = fromChurchList (getChurchListSnocFunc f snoc (toChurchList x))

snoc :: ChurchList a -> a -> ChurchList a
snoc xs x = toChurchList (fromChurchList xs ++ [x])

palinChurch :: [a] -> [a]
palinChurch = adapt f
  where
    f :: ChurchListFunc
    f = search "forall a. (forall r. (a -> r -> r) -> r -> r) -> (forall r. (a -> r -> r) -> r -> r)" $
               \(f :: ChurchListFunc) ->
                  let f' = adapt f in
                  and [
                      f' [] == ([] :: [()]),
                      f' [1,2,3] == [1,2,3,3,2,1]
                  ]
    adapt :: ChurchListFunc -> [a] -> [a]
    adapt f x = fromChurchList (getChurchListFunc f (toChurchList x))


sumSquaresChurch :: [Integer] -> Integer
sumSquaresChurch = adapt f
  where
    f = search "(forall r. ((forall z. (z -> z) -> z -> z) -> r -> r) -> r -> r) -> (forall z. (z -> z) -> z -> z)" $
              \(f :: ChurchList Church -> Church) ->
                 let f' = adapt f in
                 and [
                    f' [] == 0,
                    f' [1] == 1,
                    f' [1,2] == 5,
                    f' [1,2,4] == 21
                  ]
    adapt f x = fromChurch (f (toChurchList (map toChurch x)))

main = print $ reverseChurch [1,2,3,4,5] -- [5,4,3,2,1]
