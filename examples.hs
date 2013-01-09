{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

import Djest.Search

add2 :: Integer -> Integer
add2 = f 0 (1+)
    where
    f = search "forall Integer. Integer -> (Integer -> Integer) -> Integer -> Integer" $ \(f :: forall i. i -> (i -> i) -> i -> i) ->
        let f' = f (0 :: Integer) (1+) in
        and [
            f' 0 == 2,
            f' 2 == 4,
            f' 4 == 6
        ]

type Church = forall r. (r -> r) -> r -> r

toChurch :: Integer -> Church
toChurch 0 f x = x
toChurch n f x = f (toChurch (n-1) f x)

fromChurch :: Church -> Integer
fromChurch ch = ch succ (0 :: Integer)

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
