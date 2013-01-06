{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, KindSignatures, GADTs, DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, StandaloneDeriving #-}

module Djest.AST where

import Control.Monad.Logic
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace



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


substMeta :: MetaSubst -> Type -> Type
substMeta m = go 0
    where
    go level (t :-> u) = go level t :-> go level u
    go level (TForAll t) = TForAll (go (level + 1) t)
    go level (TMeta n) | Just t <- Map.lookup n m = raise level t
    go _ x = x


data SolverState = SolverState {
        metaSubst :: MetaSubst,
        supplyCounter :: Integer
    }

supply :: (MonadState SolverState m) => m Integer
supply = do
    s <- get
    put $ s { supplyCounter = supplyCounter s + 1 }
    return $ supplyCounter s

onMetaSubst :: (MetaSubst -> MetaSubst) -> SolverState -> SolverState
onMetaSubst f s = s { metaSubst = f (metaSubst s) }

type MetaSubst = Map.Map MetaVar Type
type Env = Map.Map Type [ExpVar]

type Solver = StateT SolverState Logic

unify :: Type -> Type -> Solver ()
unify t u = join $ liftM2 go (substMetas t) (substMetas u)
    where
    go t u | t == u = return ()
    go (TMeta m) u = modify . onMetaSubst $ Map.insert m u
    go t (TMeta m) = modify . onMetaSubst $ Map.insert m t
    go (t :-> u) (t' :-> u') = go t t' >> unify u u'  -- unify to resubstitute any new bindings
    go _ _ = mzero

substMetas :: Type -> Solver Type
substMetas t = do
    s <- gets metaSubst
    return $ substMeta s t

runSolver :: Solver a -> [a]
runSolver s = observeAll (evalStateT s (SolverState Map.empty 0))

instantiate :: Type -> Solver Type
instantiate (t :-> u) = (t :->) <$> instantiate u
instantiate (TForAll t) = do
    meta <- MetaVar <$> supply
    instantiate $ subst (TMeta meta) t
instantiate x = return x

refine :: Type -> Type -> Solver [Type]
refine (t :-> u) a = (t:) <$> refine u a
refine (TForAll t) a = do
    meta <- MetaVar <$> supply
    refine (subst (TMeta meta) t) a
refine t a = unify t a >> return []

try :: (MonadLogic m) => [m a] -> m a
try = foldr interleave mzero

type Rule = Env -> Type -> Solver Exp

infix 1 |-
(|-) :: Env -> Type -> Solver Exp
env |- t = do
    t' <- substMetas t
    try [ rule env t' | rule <- rules ]

rules :: [Rule]
rules = [rArrow, rForAll, rRefine]
    where
    rArrow env (t :-> u) = do
        --trace ("rArrow (" ++ show env ++ ") (" ++ show (t :-> u) ++ ")") $ return ()
        ev <- ExpVar <$> supply
        ELambda ev <$> (Map.insertWith (++) t [ev] env |- u)
    rArrow _ _ = mzero

    rForAll env (TForAll t) = do
        --trace ("rForAll (" ++ show env ++ ") (" ++ show (TForAll t) ++ ")") $ return ()
        rigid <- RigidVar <$> supply
        env |- subst (TRigid rigid) t    
    rForAll _ _ = mzero

    rRefine env (TMeta _) = mzero
    rRefine env t = try $ do
        --trace ("rRefine (" ++ show env ++ ") (" ++ show t ++ ")") $ return ()
        (k,vs) <- Map.toList env
        v <- vs
        return $ do
            args <- refine k t
            foldl (:$) (EVar v) <$> mapM' (env |-) args


mapM' :: (MonadLogic m) => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = f x >>- \a -> mapM' f xs >>- \as -> return (a:as)
