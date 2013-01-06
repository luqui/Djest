{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, KindSignatures, GADTs, DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, StandaloneDeriving #-}

module Djest.AST where

import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Supply
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set



newtype MetaVar = MetaVar Integer
    deriving (Eq, Ord, Show)
newtype RigidVar = RigidVar Integer
    deriving (Eq, Ord, Show)
newtype ExpVar = ExpVar Integer
    deriving (Eq, Ord, Show)

infixr 9 :->
data Type 
    = Type :-> Type
    | TForAll Type
    | TVar Int
    | TMeta MetaVar
    | TRigid RigidVar
    deriving (Eq, Ord, Show)

infixl 9 :$
data Exp
    = ELambda ExpVar Exp
    | Exp :$ Exp
    | EVar ExpVar
    deriving (Eq, Ord, Show)

raise :: Int -> Type -> Type
raise 0 = id
raise n = go 0
    where
    go level (t :-> u) = go level t :-> go level u
    go level (TForAll t) = TForAll (go (level + 1) t)
    go level (TVar z) 
        | z < level = TVar z
        | otherwise = TVar (z+n)
    go _ x = x

subst :: Type -> Type -> Type
subst new = go 0
    where
    go level (t :-> u) = go level t :-> go level u
    go level (TForAll t) = TForAll (go (level + 1) t)
    go level (TVar z) | z == level = raise level new
    go _ x = x



type MetaSubst = Map.Map MetaVar Type
type Env = Map.Map Type [ExpVar]

type Solver = SupplyT Integer (StateT MetaSubst Logic)


runSolver :: Solver a -> [a]
runSolver s = observeAll (evalStateT (evalSupplyT s [0..]) Map.empty)

instantiate :: Type -> Solver Type
instantiate (t :-> u) = (t :->) <$> instantiate u
instantiate (TForAll t) = do
    meta <- MetaVar <$> supply
    instantiate $ subst (TMeta meta) t
instantiate x = return x

refine :: Type -> Type -> Maybe [Type]
refine t a | t == a = Just []
refine (t :-> u) a = (t:) <$> refine u a
refine _ _ = Nothing

type Rule = Env -> Type -> Solver Exp

infix 1 |-
(|-) :: Env -> Type -> Solver Exp
env |- t = msum [ rule env t | rule <- rules ]

rules :: [Rule]
rules = [rArrow, rForAll, rRefine]
    where
    rArrow env (t :-> u) = do
        ev <- ExpVar <$> supply
        ELambda ev <$> (Map.insertWith (++) t [ev] env |- u)
    rArrow _ _ = mzero

    rForAll env (TForAll t) = do
        rigid <- RigidVar <$> supply
        env |- subst (TRigid rigid) t    
    rForAll _ _ = mzero

    rRefine env t = msum $ do
        (k,vs) <- Map.toList env
        v <- vs
        Just args <- return $ refine k t
        return $ foldl (:$) (EVar v) <$> mapM (env |-) args
