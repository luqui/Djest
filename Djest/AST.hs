{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Djest.AST where

import Control.Monad.Logic
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set

data Scope a = Here | Up a
    deriving (Show, Eq, Ord)

scopeElim :: b -> (a -> b) -> Scope a -> b
scopeElim h _ Here = h
scopeElim _ u (Up x) = u x

data Type a
    = Type a :-> Type a
    | TForAll (Type (Scope a))
    | TVar a
    deriving (Show, Eq, Ord)

data Exp a
    = ELam (Exp (Scope a))
    | EApp (Exp a) (Exp a)
    | EVar a
    deriving (Show, Eq, Ord)


newtype MetaVar = MetaVar Integer
    deriving (Show, Eq, Ord)

type Env a = Set.Set (Type a)

mapEnv :: (Ord a, Ord b) => (a -> b) -> Env a -> Env b
mapEnv f = (Set.map . fmap) f

data SolverState fv = SolverState {
        curVar :: Integer,
        curSubst :: Map.Map MetaVar (Meta fv)
    }

type Meta = Either MetaVar

type Solver fv = StateT (SolverState fv) Logic

alloc :: (MonadState (SolverState fv) m) => m MetaVar
alloc = do
    s <- get
    put $ s { curVar = curVar s + 1 }
    return . MetaVar $ curVar s

metaCommute :: Scope (Meta fv) -> Meta (Scope fv)
metaCommute Here = Right Here
metaCommute (Up (Left meta)) = Left meta
metaCommute (Up (Right v)) = Right (Up v)


exitSolver :: Solver (Scope fv) a -> Solver fv a
exitSolver solver = StateT $ \s0 -> do
    (a,s) <- runStateT solver undefined
    return (a, undefined)

infix 1 |-
(|-) :: forall fv. (Ord fv) => Env (Meta fv) -> Type (Meta fv) -> Solver fv ()
env |- t  | t `Set.member` env = return ()
env |- a :-> b   = Set.insert a env |- b
env |- TForAll t = exitSolver $ (mapEnv.fmap) Up env |- fmap metaCommute t
env |- TVar x    = mzero


instance Functor Scope where
    fmap f Here = Here
    fmap f (Up x) = Up (f x)

instance Applicative Scope where
    pure = return
    (<*>) = ap

instance Monad Scope where
    return = Up
    Here >>= f = Here
    Up x >>= f = f x

instance Functor Type where
    fmap f (dom :-> cod) = fmap f dom :-> fmap f cod
    fmap f (TForAll t) = TForAll ((fmap.fmap) f t)
    fmap f (TVar a) = TVar (f a)

instance Applicative Type where
    pure = return
    (<*>) = ap

instance Monad Type where
    return = TVar

    (dom :-> cod) >>= f = (dom >>= f) :-> (cod >>= f)    
    TForAll m >>= f = TForAll $ m >>= scopeElim (return Here) (fmap Up . f)
    TVar x >>= f = f x

instance Functor Exp where
    fmap f (ELam e) = ELam ((fmap.fmap) f e)
    fmap f (EApp a b) = EApp (fmap f a) (fmap f b)
    fmap f (EVar x) = EVar (f x)

instance Applicative Exp where
    pure = return
    (<*>) = ap

instance Monad Exp where
    return = EVar

    ELam m >>= f = ELam $ m >>= scopeElim (return Here) (fmap Up . f)
    EApp a b >>= f = EApp (a >>= f) (b >>= f)
    EVar x >>= f = f x
