module Semantico where

import AST
import Token
import Imprimivel
import Parser
import Imprimivel (erroDeTipo)
import Distribution.TestSuite (Result(Error))

-- funções de verificação gerais

buscarVar :: [Var] -> Id -> (Tipo, Expr)
buscarVar [] id = (TIgnore, IdVar id)
buscarVar ((id :#: (tipo,_)):xs) idVariavel
    | id == idVariavel = (tipo, IdVar id)
    | otherwise = buscarVar xs idVariavel

buscarFuncao :: [Funcao] -> Id -> (Tipo, Id)
buscarFuncao [] id = (TIgnore, id)
buscarFuncao ((id :->: (_, tipo)):xs) idFuncao
    | id == idFuncao = (tipo, id)
    | otherwise = buscarFuncao xs idFuncao

isNum t = t == TInt || t == TDouble
checkCast t1 t2 = t1 == TDouble && t2 == TInt

arExprA token (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expressao)
    | not ((isNum t1) || (isNum t2)) = do erroDeTipo ("tipo " ++ stringTipo1 ++ " em \"" ++ stringE1 ++ "\" e tipo " ++ stringTipo2 ++ " em \"" ++ stringE2 ++ "\" para operacao aritmetica") expressao
                                          return (TIgnore, expressao)
    | not (isNum t1) = do erroDeTipo ("tipo " ++ stringTipo1 ++ " em \"" ++ stringE1 ++ "\" para operacao aritmetica") expressao
                          return (TIgnore, expressao)
    | not (isNum t2) = do erroDeTipo ("tipo " ++ stringTipo2 ++ " em \"" ++ stringE2 ++ "\" para operacao aritmetica") expressao
                          return (TIgnore, expressao)
    | t1 == t2 = do return (t1, expressao)
    | checkCast t1 t2 = do avisoDeCast ("cast em double para \"" ++ stringE2 ++ "\"") expressao
                           return (t1, constrExprA token e1 (IntDouble e2))
    | checkCast t2 t1 = do avisoDeCast ("cast em double para \"" ++ stringE1 ++ "\"") expressao
                           return (t2, constrExprA token (IntDouble e1) e2)
    where
        expressao = constrExprA token e1 e2
        stringTipo1 = printTipo t1
        stringTipo2 = printTipo t2
        stringE1 = formatar e1
        stringE2 = formatar e2

arExprR token (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expressao)
    | t1 == TString && t2 == TString = do return (TString, expressao)
    | t1 == TString || t2 == TString = do erroDeTipo ("tipo " ++ stringTipo1 ++ " em \"" ++ stringE1 ++ "\" e tipo " ++ stringTipo2 ++ " em \"" ++ stringE2 ++ "\" para operacao relacional") expressao
                                          return (TIgnore, expressao)
    | not ((isNum t1) || (isNum t2)) = do erroDeTipo ("tipo " ++ stringTipo1 ++ " em \"" ++ stringE1 ++ "\" e tipo " ++ stringTipo2 ++ " em \"" ++ stringE2 ++ "\" para operacao relacional") expressao
                                          return (TIgnore, expressao)
    | not (isNum t1) = do erroDeTipo ("tipo " ++ stringTipo1 ++ " em \"" ++ stringE1 ++ "\" para operacao relacional") expressao
                          return (TIgnore, expressao)
    | not (isNum t2) = do erroDeTipo ("tipo " ++ stringTipo2 ++ " em \"" ++ stringE2 ++ "\" para operacao relacional") expressao
                          return (TIgnore, expressao)
    | t1 == t2 = do return (t1, expressao)
    | checkCast t1 t2 = do avisoDeCast ("cast em double para \"" ++ stringE2 ++ "\"") expressao
                           return (t1, constrExprR token e1 (IntDouble e2))
    | checkCast t2 t1 = do avisoDeCast ("cast em double para \"" ++ stringE1 ++ "\"") expressao
                           return (t2, constrExprR token (IntDouble e1) e2)
    where
        expressao = constrExprR token e1 e2
        stringTipo1 = printTipo t1
        stringTipo2 = printTipo t2
        stringE1 = formatar e1
        stringE2 = formatar e2    

arAtrib (t1, id) (t2, e)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expressao)
    | t1 == t2 = do return (t1, expressao)
    | checkCast t1 t2 = do avisoDeCast ("cast em double para \"" ++ stringE ++ "\" em uma atribuicao") expressao
                           return (TIgnore, Atrib id (IntDouble e))
    | checkCast t2 t1 = do avisoDeCast ("cast em int para \"" ++ stringE ++ "\" em uma atribuicao") expressao
                           return (TIgnore, Atrib id (DoubleInt e))
    | otherwise = do erroDeTipo ("incompatibilidade de tipos entre " ++ id ++ "(" ++ stringTipo1 ++ ") e " ++ stringE ++ "(" ++ stringTipo2 ++ ")") expressao
                     return (TIgnore, expressao)
    where
        expressao = Atrib id e
        stringTipo1 = printTipo t1
        stringTipo2 = printTipo t2
        stringE = formatar e

arRet (t1, id) (t2, e)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expressao)
    | t1 == t2 = do return (t1, Ret (Just e))
    | checkCast t1 t2 = do avisoDeCast ("cast em double para \"" ++ stringE ++ "\" no retorno da funcao " ++ id) expressao
                           return (TIgnore, Ret (Just (IntDouble e)))
    | checkCast t2 t1 = do avisoDeCast ("cast em int para \"" ++ stringE ++ "\" no retorno da funcao " ++ id) expressao
                           return (TIgnore, Ret (Just (DoubleInt e)))
    | otherwise = do erroDeTipo ("incompatibilidade de tipos entre a funcao \"" ++ id ++ "\" (" ++ stringTipo1 ++ ") e o retorno " ++ stringE ++ " (" ++ stringTipo2 ++ ")") expressao
                     return (TIgnore, expressao)
    where
        expressao = Ret (Just e)
        stringTipo1 = printTipo t1
        stringTipo2 = printTipo t2
        stringE = formatar e

-- verificadores de tipo
{-

Os verificadores precisam ser exaustivos. Lembre-se: a mônada passa implicitamente as mensagens de erro e o estado do código.

-}

verificarExpr tf tv (IdVar id) = do let (tipo, idvar) = buscarVar tv id
                                    if tipo /= TIgnore then return (tipo, idvar) else do {
                                                                                             errorMsg ("variavel \"" ++ id ++ "\" nao esta declarada");
                                                                                             return (TIgnore, IdVar id);
                                                                                         }

verificarExpr tf tv (Const (CInt c)) = return (TInt, Const (CInt c))   

verificarExpr tf tv (Const (CDouble c)) = return (TDouble, Const (CDouble c))

verificarExpr tf tv (Lit s) = return (TString, Lit s)

validaExpr tf tv token e1 e2 = do newE1 <- verificarExpr tf tv e1
                                  newE2 <- verificarExpr tf tv e2
                                  arExprA token newE1 newE2
    
verificarExpr tf tv (Add e1 e2) = validaExpr tf tv ADD e1 e2
verificarExpr tf tv (Sub e1 e2) = validaExpr tf tv SUB e1 e2
verificarExpr tf tv (Mul e1 e2) = validaExpr tf tv MUL e1 e2
verificarExpr tf tv (Div e1 e2) = validaExpr tf tv DIV e1 e2

verificarExpr tf tv (Neg e) = do (t, newE) <- verificarExpr tf tv e
                                 if isNum t then
                                    return (t, Neg newE)
                                 else if t == TIgnore then 
                                    return (TIgnore, Neg newE)
                                 else do
                                    erroDeTipo "operador de negativo requer tipo numerico" e
                                    return (TIgnore, Neg newE)

validaExprR tf tv token e1 e2 = do newE1 <- verificarExpr tf tv e1
                                   newE2 <- verificarExpr tf tv e2
                                   arExprR token newE1 newE2

verificarExprR tf tv (Req e1 e2) = validaExprR tf tv TEQ e1 e2
verificarExprR tf tv (Rdif e1 e2) = validaExprR tf tv DIFF e1 e2
verificarExprR tf tv (Rlt e1 e2) = validaExprR tf tv TLT e1 e2
verificarExprR tf tv (Rgt e1 e2) = validaExprR tf tv TGT e1 e2
verificarExprR tf tv (Rle e1 e2) = validaExprR tf tv LE e1 e2
verificarExprR tf tv (Rge e1 e2) = validaExprR tf tv GE e1 e2

verificarExprL tf tv (Rel exprR) = do (tipo, novaExprR) <- verificarExprR tf tv exprR
                                      return (Rel novaExprR)

verificarExprL tf tv (And e1 e2) = do newE1 <- verificarExprL tf tv e1
                                      newE2 <- verificarExprL tf tv e2
                                      return (And newE1 newE2)
                                   
verificarExprL tf tv (Or e1 e2) = do newE1 <- verificarExprL tf tv e1
                                     newE2 <- verificarExprL tf tv e2
                                     return (Or newE1 newE2)

verificarExprL tf tv (Not e) = do newE <- verificarExprL tf tv e
                                  return (Not newE)


-- teste simples para ilustrar o comportamento

testSem = do let a = Add (IdVar "a") (IdVar "b")
             let tf = []
             let tv = ["a" :#: (TInt, 0), "b" :#: (TDouble, 0)]
             let Result (s, msg, ast) = verificarExpr tf tv a
             putStr (msg)
             putStrLn (printExpr (snd ast) ++ " = " ++ show (snd ast))
