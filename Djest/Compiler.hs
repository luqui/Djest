{-# LANGUAGE ScopedTypeVariables, DeriveFunctor #-}

module Djest.Compiler where

import Control.Applicative
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Set as Set
import Data.Functor.Foldable (Fix(..), cata)
import Debug.Trace (trace)

type Exp a = Fix (RExp a)

lambda :: a -> Exp a -> Exp a
lambda v e = Fix (ELambda v e)

app :: Exp a -> Exp a -> Exp a
app f x = Fix (EApp f x)

var :: a -> Exp a
var = Fix . EVar

data RExp a r
    = ELambda a r
    | EApp r r
    | EVar a
    deriving (Show, Functor)

data Ann a f r = Ann a (f r)
    deriving (Show, Functor)

annotation :: Ann a f r -> a
annotation (Ann a _) = a

unroll :: Fix f -> f (Fix f)
unroll (Fix x) = x

annotate :: forall f r. (Functor f) => (f r -> r) -> Fix f -> Fix (Ann r f)
annotate f = cata cons
    where
    cons :: f (Fix (Ann r f)) -> Fix (Ann r f)
    cons fx = Fix $ Ann (f (fmap (annotation . unroll) fx)) fx

type FVExp a = Fix (Ann (Set.Set a) (RExp a))

withFreeVars :: (Ord a) => Exp a -> FVExp a
withFreeVars = annotate ann
    where
    ann (ELambda v fv) = Set.delete v fv
    ann (EApp a b) = Set.union a b
    ann (EVar v) = Set.singleton v
 

type Value = Any

projApp :: [(Bool,Bool)] -> Value -> Value -> Value
projApp = go . reverse
    where
    go [] f a = unsafeCoerce f a
    go ((fp,ap):bs) f a = unsafeCoerce $ go bs <$> passf <*> passa
        where
        passq True = unsafeCoerce
        passq False = const

        passf = passq fp f
        passa = passq ap a

ignore :: Int -> Value -> Value
ignore = go id
    where
    go f 0 = unsafeCoerce f const
    go f n = go (unsafeCoerce ((.).f)) (n-1)

compile :: forall a b. (Ord a, Show a) => Exp a -> Value
compile = go [] . withFreeVars
    where
    go args exp | trace ("go " ++ show args ++ " $ " ++ show exp) False = undefined
    go args exp = let Ann _ x = unroll exp in go' args x

    go' args (ELambda x body) 
        | x `Set.member` freeVars body = go (x:args) body
        | otherwise                    = ignore (length args) (go args body)
    go' args (EApp a b) = projApp (zip (keepArgs args (freeVars a)) (keepArgs args (freeVars b))) 
                          (go (filter (`Set.member` freeVars a) args) a) 
                          (go (filter (`Set.member` freeVars b) args) b)
    go' args (EVar x)
        | length args == 1 = unsafeCoerce id
        | otherwise = error $ "Wrong number of arguments passed to var clause: " ++ show args

    freeVars = annotation . unroll
    keepArgs args fv = map (`Set.member` fv) args
