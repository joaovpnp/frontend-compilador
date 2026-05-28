module Semantico where

import AST

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
    fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
    pure a = Result (False, "", a)
    Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

instance Monad Result where 
    -- return a = Result (False, "", a)
    Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')
  
errorMsg s = Result (True, "Erro:"++s++"\n", ())

warningMsg s = Result (False, "Advertencia:"++s++"\n", ())

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

isFuncaoDeclarada :: [Funcao] -> Id -> Bool
isFuncaoDeclarada [] _ = False
isFuncaoDeclarada ((id :->: (_,_)):xs) idFuncao
    | id == idFuncao = True
    | otherwise = isFuncaoDeclarada xs idFuncao

isFuncaoDeclarada :: [Funcao] -> Id -> Bool
isFuncaoDuplamenteDeclarada [] _ = False
isFuncaoDuplamenteDeclarada ((id :->: (_,_)):xs) idFuncao
    | idFuncao == id = xor True (isFuncaoDuplamenteDeclarada xs idFuncao)
    | otherwise = xor False (isFuncaoDuplamenteDeclarada xs idFuncao)

isFuncaoDeclarada :: [Var] -> Id -> Bool
isVarDeclarada [] _ = False
isVarDeclarada ((id :#: (_,_)):xs) idVariavel
    | id == idVariavel = True
    | otherwise = isVarDeclarada xs idVariavel

isFuncaoDeclarada :: [Var] -> Id -> Bool
isVarDuplamenteDeclarada [] _ = False
isVarDuplamenteDeclarada ((id :#: (_,_)):xs) idVariavel
    | idVariavel == id = xor True (isVarDuplamenteDeclarada xs idVariavel)
    | otherwise = xor False (isVarDuplamenteDeclarada xs idVariavel)

