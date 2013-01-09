{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables #-}

module Djest.Search (search) where

import qualified Djest.Solver as S
import qualified Djest.Compiler as C
import qualified Data.Map as Map
import Debug.Trace (trace)

compileExp :: S.Exp -> a
compileExp = C.compile . go
    where
    go (S.ELambda x e) = C.lambda x (go e)
    go (x S.:$ y) = C.app (go x) (go y)
    go (S.EVar v) = C.var v

search :: String -> (a -> Bool) -> a
search typeDesc tests = 
    case S.parseType typeDesc of
        Left err -> error (show err)
        Right typ ->
            case filter tests . map compileExp . map (\x -> trace (show (S.printExp x)) x) . S.runSolver $ Map.empty S.|- typ of
                [] -> error $ "Type " ++ show (S.printType typ) ++ " not satisfiable"
                (x:_) -> x
