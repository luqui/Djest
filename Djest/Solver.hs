{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, KindSignatures, GADTs, DeriveFunctor, DeriveFoldable, DeriveTraversable, RankNTypes, StandaloneDeriving, ConstraintKinds, DeriveDataTypeable #-}

module Djest.Solver where

import Prelude hiding (lex)
import Control.Monad.Logic
import Control.Monad.State
import Control.Monad.Writer
import Djest.MonadDelay
import qualified Djest.IterativeDeepening as ID
import qualified Control.Monad.Supply as Supply
import Control.Applicative
import Control.Arrow (first, second)
import Data.Tuple (swap)
import Debug.Trace (trace)
import System.Environment (getArgs)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.PrettyPrint as PP
import Data.Data

newtype MetaVar = MetaVar Integer
    deriving (Eq, Ord, Show, Data, Typeable)
data RigidVar = RigidVar Integer
    deriving (Eq, Ord, Show, Data, Typeable)
newtype ExpVar = ExpVar Integer
    deriving (Eq, Ord, Show, Data, Typeable)

renderExpVar :: ExpVar -> String
renderExpVar (ExpVar n) = "@" ++ show n

renderMetaVar :: MetaVar -> String
renderMetaVar (MetaVar n) = "?" ++ show n

renderRigidVar :: RigidVar -> String
renderRigidVar (RigidVar n) = "!" ++ show n

infixr 9 :->
infixl 9 :%
data Type
    = Type :-> Type
    | Type :% Type
    | TForAll Type
    | TVar Int
    | TMeta MetaVar
    | TRigid RigidVar
    deriving (Eq, Ord, Show, Data, Typeable)

type Parser = P.Parsec String ()
typeParser :: Parser Type
typeParser = top Map.empty
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

    top m = foldr1 (:->) <$> P.sepBy1 (app m) (P.symbol lex "->")
    app m = foldl1 (:%) <$> P.many1 (atom m)
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
printType t = Supply.evalSupply (go [] id id t) letters
    where
    go names pr pa (t :-> u) = do
        tp <- go names PP.parens id t
        up <- go names id id u
        return . pr $ tp PP.<+> PP.text "->" PP.<+> up
    go names pr pa (t :% u) = do
        tp <- go names PP.parens id t
        up <- go names PP.parens PP.parens u
        return . pa $ tp PP.<+> up
    go names pr pa t@(TForAll _) = do
        (t', bound) <- foralls t
        rest <- go (reverse bound ++ names) id id t'
        return . pr $ PP.text "forall" PP.<+> PP.hsep (map PP.text bound) PP.<> PP.text "." PP.<+> rest
    go names pr pa (TVar z) = return $ PP.text (names !! z)
    go names pr pa (TMeta v) = return $ PP.text (renderMetaVar v)
    go names pr pa (TRigid v) = return $ PP.text (renderRigidVar v)

    foralls (TForAll t) = liftM2 (second . (:)) Supply.supply (foralls t)
    foralls x = return (x, [])

    letters = map (:[]) ['a'..'z'] ++ liftM2 (:) ['a'..'z'] letters

printTurnstile :: SolverState -> Env -> Type -> PP.Doc
printTurnstile state env t = PP.vcat [ printVars vs PP.<+> PP.text ":" PP.<+> printType (substAll (metaSubst state) k) | (k,vs) <- Map.toList env ]
                         PP.$$ 
                       PP.text "|-"
                         PP.$$ 
                       printType (substAll (metaSubst state) t)
    where
    printVars vs = PP.hsep $ map (PP.text . renderExpVar) vs


parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse (p <* P.eof) "<input>"

parseType :: String -> Either P.ParseError Type
parseType = parse typeParser

mainF :: String -> IO ()
mainF input = do
    typ <- either (fail.show) return $ parse typeParser input
    mapM_ showLine . runSolver $ Map.empty |- typ

    where
    showLine x = print (printExp x) >> getLine >> return ()

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
    go names pl pa (EVar v) = return $ PP.text (Map.findWithDefault (renderExpVar v) v names)

    letters = map (:[]) ['a'..'z'] ++ liftM2 (:) ['a'..'z'] letters

raise :: Int -> Type -> Type
raise 0 = id
raise n = go 0
    where
    go level (t :-> u) = go level t :-> go level u
    go level (t :% u) = go level t :% go level u
    go level (TForAll t) = TForAll (go (level + 1) t)
    go level (TVar z)
        | z < level = TVar z
        | otherwise = TVar (z+n)
    go _ x = x

subst :: Type -> Type -> Type
subst new = go 0
    where
    go level (t :-> u) = go level t :-> go level u
    go level (t :% u) = go level t :% go level u
    go level (TForAll t) = TForAll (go (level + 1) t)
    go level (TVar z) | z == level = raise level new
    go _ x = x


substWhnf :: MetaSubst -> Type -> Type
substWhnf m (TMeta n) | Just t <- Map.lookup n m = substWhnf m t
substWhnf m t = t

substWhnf' :: (MonadState SolverState m) => Type -> m Type
substWhnf' t = flip substWhnf t <$> gets metaSubst

substAll :: MetaSubst -> Type -> Type
substAll m (t :-> u) = substAll m t :-> substAll m u
substAll m (t :% u) = substAll m t :% substAll m u
substAll m (TForAll t) = TForAll (substAll m t)
substAll m (TMeta n) | Just t <- Map.lookup n m = substAll m t
substAll m t = t

substAll' :: (MonadState SolverState m) => Type -> m Type
substAll' t = flip substAll t <$> gets metaSubst


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

-- We need a principled approach so that we always view a type through the
-- metasubst lens, without doing too much extra work.  Our substWhnf's are
-- spuriously placed around this code and if we missed one there could be a
-- very subtle problem.
type MetaSubst = Map.Map MetaVar Type
type Env = Map.Map Type [ExpVar]

type MonadSolver m = (Functor m, MonadState SolverState m, MonadDelay m, MonadWriter [PP.Doc] m)

unify :: (MonadSolver m) => Type -> Type -> m ()
unify t u = join $ liftM2 go (substWhnf' t) (substWhnf' u)
    where
    go t u | t == u = return ()
    go (TMeta m) u = setMeta m u
    go t (TMeta m) = setMeta m t
    go (t :-> u) (t' :-> u') = unify t t' >> unify u u'
    go (t :% u) (t' :% u') = unify t t' >> unify u u'
    -- TOOD unify TForAlls?
    go _ _ = mzero

    setMeta m t = do
        t' <- substAll' t
        guard $ not (occurs m t')
        tell [PP.text (renderMetaVar m ++ " :=") PP.<+> printType t']
        modify . onMetaSubst $ Map.insert m t'

    occurs m (TMeta m') | m == m' = True
    occurs m (t :-> u) = occurs m t || occurs m u
    occurs m (t :% u) = occurs m t || occurs m u
    occurs m (TForAll t) = occurs m t
    occurs _ _ = False

runSolver :: WriterT [PP.Doc] (StateT SolverState (ID.T Int)) a -> [a]
runSolver s = map traceline $ ID.flatten (evalStateT (runWriterT s) (SolverState Map.empty 0))
    where
    --traceline (r, tr) = trace (unlines (map (\x -> show x ++ "\n") tr)) r
    traceline (r, _) = r

refine :: (MonadSolver m) => Type -> Type -> m [Type]
refine t a = do
    t' <- substWhnf' t
    tell [PP.text "(refine) " PP.<+> printType t' PP.<+> PP.text "against" PP.<+> printType a]
    (unify t' a >> return []) `mplus` delay (do
        case t' of
            dom :-> cod -> (dom:) <$> refine cod a
            TForAll body -> do
                meta <- MetaVar <$> supply
                refine (subst (TMeta meta) body) a
            _ -> mzero)

type Rule m = Env -> Type -> m Exp

infix 1 |-
(|-) :: (MonadSolver m) => Rule m
env |- t = do
    t' <- substWhnf' t
    state <- get
    tell [printTurnstile state env t]
    msum [ rule env t' | rule <- rules ]

rules :: (MonadSolver m) => [Rule m]
rules = [rArrow, rForAll, rRefine]
    where
    rArrow env (t :-> u) = do
        tell [PP.text "rArrow"]
        ev <- ExpVar <$> supply
        ELambda ev <$> (Map.insertWith (++) t [ev] env |- u)
    rArrow _ _ = mzero

    rForAll env (TForAll t) = do
        tell [PP.text "rForAll"]
        rigid <- RigidVar <$> supply
        env |- subst (TRigid rigid) t
    rForAll _ _ = mzero

    rRefine env (TMeta _) = mzero
    rRefine env t = delay . msum $ do
        (k,vs) <- Map.toList env
        v <- vs
        return $ do
            args <- refine k t
            dbgType <- substAll' k
            tell [PP.text "begin rRefine " PP.<+> PP.text (renderExpVar v) PP.<+> PP.text ":" PP.<+> printType dbgType]
            proofs <- censor (map (PP.nest 2)) $ mapM (env |-) args
            let result = foldl (:$) (EVar v) proofs
            tell [PP.text "end rRefine " PP.<+> printExp result]
            return result
