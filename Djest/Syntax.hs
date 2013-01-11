module Djest.Syntax where

import Prelude hiding (lex)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Layout as PL
import qualified Data.Char as Char
import Control.Applicative
import Control.Monad (guard)

type Var = String
type TypeVar = String

data Type
    = TArrow Type Type
    | TForAll TypeVar Type
    | TApply Type Type
    | TVar Var
    deriving (Show)

data VarPat
    = VPVar Var
    deriving (Show)

data Exp
    = ELambda [VarPat] Exp
    | EApp Exp Exp
    | EVar Var
    deriving (Show)

data Assertion
    = AHasType Exp Type
    | ADefn Var [VarPat] Exp [Assertion]
    | ATest Exp
    deriving (Show)

type Parser = P.Parsec String PL.LayoutEnv

lex :: Parser a -> Parser a
lex p = PL.spaced p

tok :: Parser a -> Parser a
tok p = lex . P.try $ p

guards :: (a -> Bool) -> Parser a -> Parser a
guards f p = do
    x <- p
    guard (f x)
    return x

identifier :: Parser String
identifier = tok . guards (`notElem` reserved) $
    (:) <$> P.satisfy ((||) <$> Char.isLetter <*> (== '_')) 
        <*> P.many (P.satisfy ((||) <$> Char.isAlphaNum <*> (`elem` "'_")))

operator :: Parser String
operator = tok . guards (`notElem` reserved) $ P.many1 (P.oneOf "`~!@#$%^&*-+=|\\:<>/?")

reserved = [ "forall", "where", "=", "::", "\\" ]

symbol :: String -> Parser ()
symbol = fmap (const ()) . tok . P.string 

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

pType :: Parser Type
pType = foldr1 TArrow <$> P.many1 app
    where
    forAll = TForAll <$> (symbol "forall" *> identifier <* symbol ".") <*> pType
    app = foldl1 TApply <$> P.many1 atom
    atom = TVar <$> identifier <|> parens pType

pVarPat :: Parser VarPat
pVarPat = VPVar <$> identifier

pExp :: Parser Exp
pExp = P.choice [ lambda, opExp ]
    where
    lambda = ELambda <$> (symbol "\\" *> P.many1 pVarPat <* symbol ".") <*> pExp
    opExp = operate <$> app <*> P.optionMaybe ((,) <$> operator <*> app)
        where
        operate exp Nothing = exp
        operate exp (Just (op,exp')) = EApp (EApp (EVar op) exp) exp'
    app = foldl1 EApp <$> P.many1 atom
    atom = EVar <$> identifier <|> parens pExp

pAssertion :: Parser Assertion
pAssertion = P.choice
    [ P.try (ADefn <$> identifier <*> P.many pVarPat <* symbol "=" <*> pExp 
                   <*> P.option [] (symbol "where" *> PL.laidout pAssertion))
    , flip ($) <$> pExp <*> 
            P.choice [
                flip AHasType <$> (symbol "::" *> pType),
                pure ATest
            ]
    ]

pProgram :: Parser [Assertion]
pProgram = PL.laidout pAssertion

parse :: String -> Either P.ParseError [Assertion]
parse s = P.runParser pProgram PL.defaultLayoutEnv "<input>" s
