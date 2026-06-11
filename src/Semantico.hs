module Semantico where

import AST
import Token
import Imprimivel
import Parser
import Distribution.TestSuite (Result(Error))
import Text.XHtml (blockquote)
import qualified Lex as L
import Data.Char (digitToInt)

-- funções de verificação gerais

duplicados :: Eq a => [a] -> [a]
duplicados [] = []
duplicados (x:xs)
    | x `elem` xs = x : duplicados (filter (/= x) xs)
    | otherwise   = duplicados xs

verificarVarsDuplicadas :: String -> [Var] -> Imprimivel.Result ()
verificarVarsDuplicadas escopo vars = do
    let ids = map (\(idV :#: _) -> idV) vars
    let dups = duplicados ids
    mapM_ (\idV -> errorMsg ("variavel \"" ++ idV ++ "\" declarada multiplas vezes na funcao " ++ escopo)) dups

verificarFuncsDuplicadas :: [Funcao] -> Imprimivel.Result ()
verificarFuncsDuplicadas funcs = do
    let ids = map (\(idF :->: _) -> idF) funcs
    let dups = duplicados ids
    mapM_ (\idF -> errorMsg ("funcao \"" ++ idF ++ "\" declarada multiplas vezes")) dups

buscarVar :: [Var] -> Id -> (Tipo, Expr)
buscarVar [] id = (TIgnore, IdVar id)
buscarVar ((id :#: (tipo,_)):xs) idVariavel
    | id == idVariavel = (tipo, IdVar id)
    | otherwise = buscarVar xs idVariavel

buscarFuncao :: [Funcao] -> Id -> (Tipo, Id, [Var])
buscarFuncao [] id = (TIgnore, id, [])
buscarFuncao ((id :->: (pars, tipo)):xs) idFuncao
    | id == idFuncao = (tipo, id, pars)
    | otherwise = buscarFuncao xs idFuncao

isNum :: Tipo -> Bool
isNum t = t == TInt || t == TDouble

checkCast :: Tipo -> Tipo -> Bool
checkCast t1 t2 = t1 == TDouble && t2 == TInt

arExprA :: Imprimivel p => Id -> p -> Token -> (Tipo, Expr) -> (Tipo, Expr) -> Imprimivel.Result (Tipo, Expr)
arExprA nomeF cmd token (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expr)
    | isNum t1 && t1 == t2           = return (t1, expr)
    | checkCast t1 t2                = avisoDeCast (msgCast e2) nomeF cmd expr >> return (t1, constrExprA token e1 (IntDouble e2))
    | checkCast t2 t1                = avisoDeCast (msgCast e1) nomeF cmd expr >> return (t2, constrExprA token (IntDouble e1) e2)
    | otherwise                      = erroDeTipo msgErro nomeF cmd expr >> return (TIgnore, expr)
    where
        expr     = constrExprA token e1 e2
        msgCast e = "cast para double em \"" ++ formatar e ++ "\""
        msgErro
            | not (isNum t1) && not (isNum t2) = "tipo " ++ printTipo t1 ++ " em \"" ++ formatar e1 ++ "\" e tipo " ++ printTipo t2 ++ " em \"" ++ formatar e2 ++ "\" para operacao aritmetica"
            | not (isNum t1)                   = "tipo " ++ printTipo t1 ++ " em \"" ++ formatar e1 ++ "\" para operacao aritmetica"
            | otherwise                        = "tipo " ++ printTipo t2 ++ " em \"" ++ formatar e2 ++ "\" para operacao aritmetica"

arExprR nomeF cmd token (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expr)
    | t1 == TString && t2 == TString = return (TString, expr)
    | isNum t1 && t1 == t2           = return (t1, expr)
    | checkCast t1 t2                = avisoDeCast (msgCast e2) nomeF cmd expr >> return (t1, constrExprR token e1 (IntDouble e2))
    | checkCast t2 t1                = avisoDeCast (msgCast e1) nomeF cmd expr >> return (t2, constrExprR token (IntDouble e1) e2)
    | otherwise                      = erroDeTipo msgErro nomeF cmd expr >> return (TIgnore, expr)
    where
        expr     = constrExprR token e1 e2
        msgCast e = "cast para double em \"" ++ formatar e ++ "\""
        msgErro
            | t1 == TString || t2 == TString   = "tipo " ++ printTipo t1 ++ " em \"" ++ formatar e1 ++ "\" e tipo " ++ printTipo t2 ++ " em \"" ++ formatar e2 ++ "\" para operacao relacional"
            | not (isNum t1) && not (isNum t2) = "tipo " ++ printTipo t1 ++ " em \"" ++ formatar e1 ++ "\" e tipo " ++ printTipo t2 ++ " em \"" ++ formatar e2 ++ "\" para operacao relacional"
            | not (isNum t1)                   = "tipo " ++ printTipo t1 ++ " em \"" ++ formatar e1 ++ "\" para operacao relacional"
            | otherwise                        = "tipo " ++ printTipo t2 ++ " em \"" ++ formatar e2 ++ "\" para operacao relacional"

arAtrib nomeF cmd (t1, id) (t2, e)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expr)
    | t1 == t2                       = return (t1, expr)
    | checkCast t1 t2                = avisoDeCast msgCastD nomeF cmd expr >> return (TIgnore, Atrib id (IntDouble e))
    | checkCast t2 t1                = avisoDeCast msgCastI nomeF cmd expr >> return (TIgnore, Atrib id (DoubleInt e))
    | otherwise                      = erroDeTipo msgErro nomeF cmd expr >> return (TIgnore, expr)
    where
        expr     = Atrib id e
        msgCastD = "cast para double em \"" ++ formatar e ++ "\""
        msgCastI = "cast para int em \"" ++ formatar e ++ "\""
        msgErro  = "incompatibilidade de tipos entre a funcao " ++ id ++ " (do tipo " ++ printTipo t1 ++ ") e " ++ formatar e ++ "(do tipo " ++ printTipo t2 ++ ")"

arRet nomeF cmd (t1, id) (t2, Nothing)
    | t1 == TIgnore || t1 == TVoid = return (t1, Ret Nothing)
    | otherwise                    = erroDeTipo msgErro nomeF cmd (Ret Nothing) >> return (TIgnore, Ret Nothing)
    where
        msgErro  = "incompatibilidade de tipos entre a funcao " ++ id ++ " (do tipo " ++ printTipo t1 ++ ") e o retorno (do tipo " ++ printTipo t2 ++ ")"

arRet nomeF cmd (t1, id) (t2, Just e)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, expr)
    | t1 == t2                       = return (t1, expr)
    | checkCast t1 t2                = avisoDeCast msgCastD nomeF cmd expr >> return (TIgnore, Ret (Just (IntDouble e)))
    | checkCast t2 t1                = avisoDeCast msgCastI nomeF cmd expr >> return (TIgnore, Ret (Just (DoubleInt e)))
    | otherwise                      = erroDeTipo msgErro nomeF cmd expr >> return (TIgnore, expr)
    where
        expr     = Ret (Just e)
        msgCastD = "cast para double em \"" ++ formatar e ++ "\" no retorno da funcao " ++ id
        msgCastI = "cast para int em \"" ++ formatar e ++ "\" no retorno da funcao " ++ id
        msgErro  = "incompatibilidade de tipos entre a funcao " ++ id ++ " (do tipo " ++ printTipo t1 ++ ") e o retorno (do tipo " ++ printTipo t2 ++ ")"

arChamada nomeF cmd funcao chamada (t1, e1) (t2, e2)
    | t1 == TIgnore || t2 == TIgnore = return (TIgnore, e2)
    | t1 == t2                       = return (t1, e2)
    | checkCast t1 t2                = avisoDeCast msgCastD nomeF cmd chamada >> return (TDouble, IntDouble e2)
    | checkCast t2 t1                = avisoDeCast msgCastI nomeF cmd chamada >> return (TInt, DoubleInt e2)
    | otherwise                      = erroDeTipo msgErro nomeF cmd chamada >> return (TIgnore, e2)
    where
        msgCastD = "cast para double em \"" ++ formatar e2 ++ "\" pois eh um parametro da funcao " ++ printAssinaturaFuncao funcao
        msgCastI = "cast para int em \"" ++ formatar e2 ++ "\" pois eh um parametro da funcao " ++ printAssinaturaFuncao funcao
        msgErro  = "incompatibilidade de tipos entre parametros da funcao " ++ printAssinaturaFuncao funcao ++ " e o parametro " ++ formatar e2 ++ " (do tipo " ++ printTipo t2 ++ ")"

-- verificadores de tipo

validaExpr :: [Funcao] -> [Var] -> Id -> Comando -> Token -> Expr -> Expr -> Imprimivel.Result (Tipo, Expr)
validaExpr tf tv idFuncao cmd token e1 e2 = do
    newE1 <- verificarExpr tf tv idFuncao cmd e1
    newE2 <- verificarExpr tf tv idFuncao cmd e2
    arExprA idFuncao cmd token newE1 newE2


comparaTipos idFuncao cmd f c [] [] = return []

comparaTipos idFuncao cmd f c xs [] = do 
    errorMsg ("na funcao " ++ idFuncao ++ " \n\t-> no comando: " ++ formatar cmd ++ "\n\t\t -> quantidade invalida de parametros para a funcao " ++ formatar f)
    return []

comparaTipos idFuncao cmd f c [] ys = do 
    errorMsg ("na funcao " ++ idFuncao ++ " \n\t-> no comando: " ++ formatar cmd ++ "\n\t\t -> quantidade invalida de parametros para a funcao " ++ formatar f)
    return []

comparaTipos idFuncao cmd f c (x:xs) (y:ys) = do
    restoNovos <- comparaTipos idFuncao cmd f c xs ys
    (_, exprNova) <- arChamada idFuncao cmd f c x y
    return (exprNova : restoNovos)

verificarExpr :: [Funcao] -> [Var] -> Id -> Comando -> Expr -> Imprimivel.Result (Tipo, Expr)
verificarExpr tf tv idFuncao cmd (Chamada id parametros) = do 
    let (tipoRetorno, nome, parametrosEsperados) = buscarFuncao tf id 
    if tipoRetorno == TIgnore then do
        errorMsg ("funcao \"" ++ nome ++ "\" nao esta declarada")
        return (TIgnore, Chamada id parametros)
    else do
        pEsperadosAvaliados <- mapM (verificarExpr tf parametrosEsperados idFuncao cmd . (\(id :#: (_,_)) -> IdVar id)) parametrosEsperados
        parametrosAvaliados <- mapM (verificarExpr tf tv idFuncao cmd) parametros
        novosParametros <- comparaTipos idFuncao cmd (nome :->: (parametrosEsperados, tipoRetorno)) (Chamada id parametros) pEsperadosAvaliados parametrosAvaliados
        return (tipoRetorno, Chamada id novosParametros)

verificarExpr tf tv idFuncao cmd (IdVar id) = do 
    let (tipo, idvar) = buscarVar tv id
    if tipo /= TIgnore then return (tipo, idvar)
    else do
        errorMsg ("variavel \"" ++ id ++ "\" nao esta declarada");
        return (TIgnore, IdVar id);

verificarExpr tf tv idFuncao cmd (Const (CInt c)) = return (TInt, Const (CInt c))   

verificarExpr tf tv idFuncao cmd (Const (CDouble c)) = return (TDouble, Const (CDouble c))

verificarExpr tf tv idFuncao cmd (Lit s) = return (TString, Lit s)
    
verificarExpr tf tv idFuncao cmd (Add e1 e2) = validaExpr tf tv idFuncao cmd ADD e1 e2
verificarExpr tf tv idFuncao cmd (Sub e1 e2) = validaExpr tf tv idFuncao cmd SUB e1 e2
verificarExpr tf tv idFuncao cmd (Mul e1 e2) = validaExpr tf tv idFuncao cmd MUL e1 e2
verificarExpr tf tv idFuncao cmd (Div e1 e2) = validaExpr tf tv idFuncao cmd DIV e1 e2

verificarExpr tf tv idFuncao cmd (Neg e) = do 
    (t, newE) <- verificarExpr tf tv idFuncao cmd e
    if isNum t then
        return (t, Neg newE)
    else if t == TIgnore then 
        return (TIgnore, Neg newE)
    else do
        erroDeTipo ("incompatibilidade de tipo (" ++ printTipo t ++ ")") idFuncao cmd (Neg e)
        return (TIgnore, Neg newE)

validaExprR :: [Funcao] -> [Var] -> Id -> Comando -> Token -> Expr -> Expr -> Imprimivel.Result (Tipo, ExprR)
validaExprR tf tv idFuncao cmd token e1 e2 = do newE1 <- verificarExpr tf tv idFuncao cmd e1
                                                newE2 <- verificarExpr tf tv idFuncao cmd e2
                                                arExprR idFuncao cmd token newE1 newE2

verificarExprR :: [Funcao] -> [Var] -> Id -> Comando -> ExprR -> Imprimivel.Result (Tipo, ExprR)
verificarExprR tf tv idFuncao cmd (Req e1 e2) = validaExprR tf tv idFuncao cmd TEQ e1 e2
verificarExprR tf tv idFuncao cmd (Rdif e1 e2) = validaExprR tf tv idFuncao cmd DIFF e1 e2
verificarExprR tf tv idFuncao cmd (Rlt e1 e2) = validaExprR tf tv idFuncao cmd TLT e1 e2
verificarExprR tf tv idFuncao cmd (Rgt e1 e2) = validaExprR tf tv idFuncao cmd TGT e1 e2
verificarExprR tf tv idFuncao cmd (Rle e1 e2) = validaExprR tf tv idFuncao cmd LE e1 e2
verificarExprR tf tv idFuncao cmd (Rge e1 e2) = validaExprR tf tv idFuncao cmd GE e1 e2

verificarExprL :: [Funcao] -> [Var] -> Id -> Comando -> ExprL -> Imprimivel.Result ExprL
verificarExprL tf tv idFuncao cmd (Rel exprR) = do (tipo, novaExprR) <- verificarExprR tf tv idFuncao cmd exprR
                                                   return (Rel novaExprR)

verificarExprL tf tv idFuncao cmd (And e1 e2) = do newE1 <- verificarExprL tf tv idFuncao cmd e1
                                                   newE2 <- verificarExprL tf tv idFuncao cmd e2
                                                   return (And newE1 newE2)
                                   
verificarExprL tf tv idFuncao cmd (Or e1 e2) = do newE1 <- verificarExprL tf tv idFuncao cmd e1
                                                  newE2 <- verificarExprL tf tv idFuncao cmd e2
                                                  return (Or newE1 newE2)

verificarExprL tf tv idFuncao cmd (Not e) = do newE <- verificarExprL tf tv idFuncao cmd e
                                               return (Not newE)

verificarComando :: [Funcao] -> [Var] -> Tipo -> [Char] -> Comando -> Imprimivel.Result Comando
verificarComando tf tv tipof nomef (Imp expr) = do (_, newE) <- verificarExpr tf tv nomef (Imp expr) expr
                                                   return (Imp newE)

verificarComando tf tv tipof nomef (Leitura id) = do _ <- verificarExpr tf tv nomef (Leitura id) (IdVar id)
                                                     return (Leitura id)

verificarComando tf tv tipof nomef (While cond bloco) = do novaCond <- verificarExprL tf tv nomef (While cond []) cond
                                                           novoBloco <- mapM (verificarComando tf tv tipof nomef) bloco
                                                           return (While novaCond novoBloco)

verificarComando tf tv tipof nomef (If cond b1 b2) = do novaCond <- verificarExprL tf tv nomef (If cond [] []) cond
                                                        newB1 <- mapM (verificarComando tf tv tipof nomef) b1 
                                                        newB2 <- mapM (verificarComando tf tv tipof nomef) b2
                                                        return (If novaCond newB1 newB2)

verificarComando tf tv tipof nomef (Atrib id expr) = do (tipoVar, _) <- verificarExpr tf tv nomef (Atrib id expr) (IdVar id)
                                                        novaExpr <- verificarExpr tf tv nomef (Atrib id expr) expr
                                                        (_, novoComando) <- arAtrib nomef (Atrib id expr) (tipoVar, id) novaExpr   
                                                        return novoComando

verificarComando tf tv tipof nomef (Ret Nothing) = do (t, e) <- arRet nomef (Ret Nothing) (tipof, nomef) (TVoid, Nothing) 
                                                      return e

verificarComando tf tv tipof nomef (Ret (Just expr)) = do (tipoExpr, e) <- verificarExpr tf tv nomef (Ret (Just expr)) expr
                                                          (t, e2) <- arRet nomef (Ret (Just expr)) (tipof, nomef) (tipoExpr, Just e)
                                                          return e2

verificarComando tf tv tipof nomef (Proc id expr) = do (_, exprAvaliada) <- verificarExpr tf tv nomef (Proc id expr) (Chamada id expr)
                                                       case exprAvaliada of
                                                           Chamada _ exprsAvaliadas -> return (Proc id exprsAvaliadas)
                                                           _                        -> return (Proc id expr)

verificarFuncao tf (id, tv, bloco) = do 
    let (tipoRetorno, nome, parametrosEsperados) = buscarFuncao tf id
    verificarVarsDuplicadas nome (parametrosEsperados ++ tv)
    if tipoRetorno == TIgnore then do
        errorMsg ("funcao \"" ++ nome ++ "\" nao esta declarada")
        return (id, tv, bloco)
    else do
        novoBloco <- mapM (verificarComando tf (parametrosEsperados ++ tv) tipoRetorno nome) bloco
        return (id, tv, novoBloco)

verificarPrograma :: Programa -> Imprimivel.Result Programa
verificarPrograma (Prog tf codFuncoes varMain codigoPrincipal) = do
    verificarFuncsDuplicadas tf
    novasFuncoes <- mapM (verificarFuncao tf) codFuncoes
    novaMain <- mapM (verificarComando tf varMain TVoid "principal") codigoPrincipal
    return (Prog tf novasFuncoes varMain novaMain)

semantico :: Programa -> IO ()
semantico prog = do 
    let Result (status, mensagem, novoProg) = verificarPrograma prog
    if status == True then do
        putStrLn "Erro de compilacao\n"
    else do
        putStrLn "Pronto para gerar codigo intermediario\n"
    putStrLn mensagem
    putStrLn "\nNovo programa:\n\n"
    putStrLn (printProg novoProg)

-- teste simples para ilustrar o comportamento

testSem = do
    file <- readFile "teste.j--"
    let prog = calc (L.alexScanTokens file)
    putStrLn ("Programa\n\n" ++ printProg prog ++ "\n")
    semantico prog
    

    
