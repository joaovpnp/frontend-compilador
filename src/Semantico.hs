module Semantico where

import AST
import Token
import Imprimivel
import Parser
import Imprimivel (erroDeTipo)
import Distribution.TestSuite (Result(Error))
import Text.XHtml (blockquote)

-- funções de verificação gerais

buscarVar :: [Var] -> Id -> (Tipo, Expr)
buscarVar [] id = (TIgnore, IdVar id)
buscarVar ((id :#: (tipo,_)):xs) idVariavel
    | id == idVariavel = (tipo, IdVar id)
    | otherwise = buscarVar xs idVariavel

buscarFuncao :: [Funcao] -> Id -> (Tipo, Id, [Var])
buscarFuncao [] id = (TIgnore, id)
buscarFuncao ((id :->: (pars, tipo)):xs) idFuncao
    | id == idFuncao = (tipo, id, pars)
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

arRet (t1, id) (t2, Nothing)
    | t1 == TIgnore || t1 == TVoid = return (t1, Ret Nothing)
    | otherwise = do erroDeTipo ("incompatibilidade de tipos entre a funcao \"" ++ id ++ "\" (" ++ stringTipo1 ++ ") e o retorno " ++ stringE ++ " (" ++ stringTipo2 ++ ")") expressao
                     return (TIgnore, Ret Nothing)
arRet (t1, id) (t2, Just e)
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

arChamada funcao chamada (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, e2)
    | t1 == t2 = do return (t1, e2)
    | checkCast t1 t2 = do avisoDeCast ("cast em double para \"" ++ stringE2 ++ "\" como parametro da funcao " ++ printAssinaturaFuncao funcao) chamada
                           return (TDouble, IntDouble e2)
    | otherwise = do erroDeTipo ("incompatibilidade de tipos entre parametros da funcao \"" ++ printAssinaturaFuncao funcao ++ " e o parametro " ++ stringE ++ " (" ++ stringTipo2 ++ ")") chamada
                     return (TIgnore, e2)
    where
        stringTipo2 = printTipo t2
        stringE2 = formatar e2

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

comparaTipos f c [] [] = return []
comparaTipos f c xs [] = do errorMsg (Quantidade de parametros invalida para a funcao ++ printAssinaturaFuncao f ++ na expressao ++ formatar c
comparaTipos f c (x:xs) (y:ys) = return (arChamada f c x y : comparaTipos f c xs ys)

verificarExpr tf tv (Chamada id parametros) = do (tipoRetorno, nome, parametrosEsperados) <- buscarFuncao tf id 
                                           if tipoRetorno == TIgnore then do 
                                              errorMsg ("funcao \"" ++ nome ++ "\" nao esta declarada")
                                              return (TIgnore, Chamada id parametros)
                                           else do
                                              parametrosAvaliados <- mapM (verificarExpr tf tv) parametros
                                              novosParametros <- comparaTipos (nome :->: (parametrosEsperados, tipo)) (Chamada id parametros) parametrosEsperados parametrosAvaliados
                                              return (tipoRetorno, Chamada id novosParametros)


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

verificarComando tf tv tipof nomef (Imp expr) = do (_, newE) <- verificarExpr tf tv expr
                                                   return (Imp newE)

verificarComando tf tv tipof nomef (Leitura id) = do _ <- verificarExpr tf tv (IdVar id)
                                                     return (Leitura id)

verificarComando tf tv tipof nomef (While cond bloco) = do novaCond <- verificarExprL tf tv cond
                                                           novoBloco <- mapM (verificarComando tf tv tipof nomef) bloco
                                                           return (While novaCond novoBloco)

verificarComando tf tv tipof nomef (If cond b1 b2) = do novaCond <- verificarExprL tf tv cond
                                                        newB1 <- mapM (verificarComando tf tv tipof nomef) b1 
                                                        newB2 <- mapM (verificarComando tf tv tipof nomef) b2
                                                        return (If novaCond newB1 newB2)

verificarComando tf tv tipof nomef (Atrib id expr) = do (tipoVar, _) <- verificarExpr tf tv (IdVar id)
                                                        novaExpr <- verificarExpr tf tv expr
                                                        (_, novoComando) <- arAtrib (tipoVar, id) novaExpr   
                                                        return novoComando

verificarComando tf tv tipof nomef (Ret Nothing) = do return arRet (tipof, nomef) (TVoid, Nothing) 

verificarComando tf tv tipof nomef (Ret (Just expr)) = do (tipoExpr, e) <- validaExpr tf tv expr
                                                          return arRet (tipof, nomef) (tipoExpr, Just e)

verificarComando tf tv tipof nomef (Proc id expr) = do verificarExpr tf tv (Chamada id expr)

-- teste simples para ilustrar o comportamento

testSem = do let a = Add (IdVar "a") (IdVar "b")
             let tf = []
             let tv = ["a" :#: (TInt, 0), "b" :#: (TDouble, 0)]
             let Result (s, msg, ast) = verificarExpr tf tv a
             putStr (msg)
             putStrLn (printExpr (snd ast) ++ " = " ++ show (snd ast))
