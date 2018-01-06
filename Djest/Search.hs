{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}

module Djest.Search (search, define) where

import qualified Djest.Solver as S
import qualified Djest.Compiler as C
import qualified Data.Map as Map
import qualified Djest.Syntax as Syn
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Map as Map
import Control.Applicative (liftA2)
import Debug.Trace (trace)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Types (Any)

define :: String -> TH.Q TH.Type -> TH.Q TH.Exp -> TH.Q [TH.Dec]
define namestr qtype qtests = do
    let name = TH.mkName namestr
    typ <- qtype
    t <- decodeType typ

    typeName <- TH.newName $ "TypeOf_" ++ namestr
    conName  <- TH.newName $ "TypeOf_" ++ namestr
    accName  <- TH.newName $ "getTypeOf_" ++ namestr

    let typedecs = [
            TH.NewtypeD [] typeName [] Nothing 
                (TH.RecC conName [(accName, TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, typ)]) [] ]

    testsName <- TH.newName "tests"
    testsParamName <- TH.newName $ "param_" ++ namestr
    testsBody <- qtests
    Just boolname <- TH.lookupTypeName "Bool"
    let testsDecs =
            [ TH.SigD testsName (TH.AppT (TH.AppT TH.ArrowT (TH.ConT typeName)) (TH.ConT boolname))
            , TH.FunD testsName [TH.Clause [TH.VarP testsParamName] (TH.NormalB testsBody) 
                [ TH.SigD name typ
                , TH.FunD name [
                    TH.Clause [] (TH.NormalB (TH.AppE (TH.VarE accName) (TH.VarE testsParamName))) []
                    ]
                ]]
            ]

    insertExp <- [| $(return (TH.VarE accName)) (searchT $(TH.liftData t) $(return (TH.VarE testsName))) |]
    return $ typedecs ++ [TH.SigD name typ, TH.FunD name [TH.Clause [] (TH.NormalB insertExp) testsDecs]]

decodeType :: TH.Type -> TH.Q S.Type
decodeType = go Map.empty
    where
    go env (TH.ForallT [] cx body)
        | not (null cx) = fail "Typeclass contexts are not yet supported"
        | otherwise     = go env body
    go env (TH.ForallT (TH.PlainTV v:vars) cx body) =
        S.TForAll <$> go (Map.insert v 0 (succ <$> env)) (TH.ForallT vars cx body)
    go env (TH.ForallT (v:vars) cx body) =
        fail $ "Unsupported binding style " ++ show v

    go env (TH.AppT (TH.AppT TH.ArrowT t) t') = liftA2 (S.:->) (go env t) (go env t')

    go env (TH.AppT t t') = liftA2 (S.:%) (go env t) (go env t')

    go env (TH.VarT n)
        | Just dbi <- Map.lookup n env = return (S.TVar dbi)
        | otherwise = fail $ "Unknown variable " ++ show n ++ " (only quantified variables are currently supported)"
    
    go env (TH.ParensT t) = go env t

    go env t = fail $ "Unsupported construct " ++ show t


encodeExp :: S.Exp -> TH.Q TH.Exp
encodeExp = go Map.empty
    where
    go env (S.ELambda v e) = do
        name <- TH.newName "v"
        TH.LamE [TH.VarP name] <$> go (Map.insert v name env) e
    go env (e S.:$ e') = 
        liftA2 TH.AppE (go env e) (go env e')
    go env (S.EVar v)
        | Just name <- Map.lookup v env = return (TH.VarE name)
        | otherwise =
            fail $ "Variable " ++ show v ++ " not in environment"

searchT :: S.Type -> (a -> Bool) -> a
searchT typ tests =
    case filter tests . map compileExp . map (\x -> trace (show (S.printExp x)) x) . S.runSolver $ Map.empty S.|- typ of
        [] -> error $ "Type " ++ show (S.printType typ) ++ " not satisfiable"
        (x:_) -> x

search :: String -> (a -> Bool) -> a
search typeDesc tests =
    case S.parseType typeDesc of
        Left err -> error (show err)
        Right typ -> searchT typ tests

compileExp :: S.Exp -> a
compileExp = C.compile . go
    where
    go (S.ELambda x e) = C.lambda x (go e)
    go (x S.:$ y) = C.app (go x) (go y)
    go (S.EVar v) = C.var v
