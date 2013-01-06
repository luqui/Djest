{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, KindSignatures, GADTs, DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, StandaloneDeriving #-}

module Djest.AST where

import Prelude hiding (lex)
import Control.Monad.Logic
import Control.Monad.State
import qualified Control.Monad.Supply as Supply
import Control.Applicative
import Control.Arrow (second)
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.PrettyPrint as PP



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

type Parser = P.Parsec String ()

parseType :: Parser Type
parseType = top Map.empty
    where
    lex = P.makeTokenParser $ P.LanguageDef {
        P.commentStart = "{-",
        P.commentEnd = "-}",
        P.commentLine = "--",
        P.nestedComments = True,
        P.identStart = P.letter,
        P.identLetter = P.alphaNum,
        P.opStart = mzero,
        P.opLetter = mzero,
        P.reservedNames = ["forall"],
        P.reservedOpNames = [],
        P.caseSensitive = True
    }
    
    top m = foldr1 (:->) <$> P.sepBy1 (atom m) (P.symbol lex "->")
    atom m = P.choice [ 
        P.reserved lex "forall" *> forAllVars m,
        P.identifier lex >>= \name -> case Map.lookup name m of
                                         Nothing -> fail $ "Type variable not in scope: " ++ name
                                         Just n -> return $ TVar n,
        P.parens lex (top m)
        ]
    forAllVars m = P.choice [ 
        P.dot lex *> top m,
        P.identifier lex >>= \name -> TForAll <$> forAllVars (Map.insert name 0 . Map.map succ $ m)
        ]

printType :: Type -> PP.Doc
printType t = Supply.evalSupply (go [] id t) letters
    where
    go names p (t :-> u) = do
        tp <- go names PP.parens t
        up <- go names id u
        return . p $ tp PP.<+> PP.text "->" PP.<+> up
    go names p t@(TForAll _) = do
        (t', bound) <- foralls t
        rest <- go (reverse bound ++ names) id t'
        return . p $ PP.text "forall" PP.<+> PP.hsep (map PP.text bound) PP.<> PP.text "." PP.<+> rest
    go names p (TVar z) = return $ PP.text (names !! z)
    go names p (TMeta z) = return $ PP.text ("?" ++ show z)
    go names p (TRigid z) = return $ PP.text ("!" ++ show z)

    foralls (TForAll t) = liftM2 (second . (:)) Supply.supply (foralls t)
    foralls x = return (x, [])

    letters = map (:[]) ['a'..'z'] ++ liftM2 (:) ['a'..'z'] letters



parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse (p <* P.eof) "<input>"

mainF :: String -> IO ()
mainF input = do
    typ <- either (fail.show) return $ parse parseType input
    go . runSolver $ Map.empty |- typ
    
    where
    go (Nothing:xs) = go xs
    go (Just x:xs) = print (printExp x) >> getLine >> go xs
    go [] = return ()

main :: IO ()
main = do
    [input] <- getArgs
    mainF input

infixl 9 :$
data Exp
    = ELambda ExpVar Exp
    | Exp :$ Exp
    | EVar ExpVar
    deriving (Eq, Ord, Show)

printExp :: Exp -> PP.Doc
printExp e = Supply.evalSupply (go Map.empty id id e) letters
    where
    go names pl pa (ELambda v e) = do
        name <- Supply.supply
        body <- go (Map.insert v name names) id id e
        return . pl $ PP.text "\\" PP.<> PP.text name PP.<> PP.text "." PP.<+> body
    go names pl pa (f :$ x) = do
        fp <- go names PP.parens id f
        xp <- go names PP.parens PP.parens x
        return . pa $ fp PP.<+> xp
    go names pl pa (EVar v) = return $ PP.text (names Map.! v)
    
    letters = map (:[]) ['a'..'z'] ++ liftM2 (:) ['a'..'z'] letters

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


substWhnf :: MetaSubst -> Type -> Type
substWhnf m (TMeta n) | Just t <- Map.lookup n m = t
substWhnf m t = t

substWhnf' :: Type -> Solver Type
substWhnf' t = do
    s <- gets metaSubst
    return $ substWhnf s t


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
unify t u = join $ liftM2 go (substWhnf' t) (substWhnf' u)
    where
    go t u | t == u = return ()
    go (TMeta m) u = modify . onMetaSubst $ Map.insert m u
    go t (TMeta m) = modify . onMetaSubst $ Map.insert m t
    go (t :-> u) (t' :-> u') = unify t t' >> unify u u'
    go _ _ = mzero

runSolver :: Solver a -> [a]
runSolver s = observeAll (evalStateT s (SolverState Map.empty 0))

instantiate :: Type -> Solver Type
instantiate (t :-> u) = (t :->) <$> instantiate u
instantiate (TForAll t) = do
    meta <- MetaVar <$> supply
    instantiate $ subst (TMeta meta) t
instantiate x = return x

refine :: Type -> Type -> Solver [Type]
refine t a = (unify t a >> return []) `interleave` do
        t' <- substWhnf' t
        go t' a
    where
    go (t :-> u) a = (t:) <$> refine u a
    go (TForAll t) a = do
        meta <- MetaVar <$> supply
        refine (subst (TMeta meta) t) a
    go _ _ = mzero

try :: (MonadLogic m) => [m a] -> m a
try = foldr interleave mzero

type Rule = Env -> Type -> Solver (Maybe Exp)

infix 1 |-
(|-) :: Env -> Type -> Solver (Maybe Exp)
env |- t = return Nothing `mplus` do
    t' <- substWhnf' t
    try [ rule env t' | rule <- rules ]

rules :: [Rule]
rules = [rArrow, rForAll, rRefine]
    where
    rArrow env (t :-> u) = do
        ev <- ExpVar <$> supply
        fmap (ELambda ev) <$> (Map.insertWith (++) t [ev] env |- u)
    rArrow _ _ = mzero

    rForAll env (TForAll t) = do
        rigid <- RigidVar <$> supply
        env |- subst (TRigid rigid) t    
    rForAll _ _ = mzero

    rRefine env (TMeta _) = mzero
    rRefine env t = try $ do
        (k,vs) <- Map.toList env
        v <- vs
        return $ do
            args <- refine k t
            proofs <- mapM' (env |-) args
            return $ foldl (:$) (EVar v) <$> sequence proofs


mapM' :: (MonadLogic m) => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = f x >>- \a -> mapM' f xs >>- \as -> return (a:as)
