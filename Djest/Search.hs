{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables #-}

module Djest.Search where

import qualified Djest.Solver as S
import qualified Djest.Compiler as C
import qualified Data.Map as Map

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
            case filter tests . map compileExp . S.runSolver $ Map.empty S.|- typ of
                [] -> error $ "Type " ++ show (S.printType typ) ++ " not satisfiable"
                (x:_) -> x


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
