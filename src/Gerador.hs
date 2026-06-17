module Gerador where

import AST
import Data.List (intercalate)
import qualified Semantico as S
import qualified Lex as L
import qualified Parser as P

import Control.Monad.State
import Data.List (genericDrop)
import Data.Char (toLower)
import GHC.IO.Handle.FD (openBinaryFile)

buscarVar :: [Var] -> Id -> Var
buscarVar [] id = ("" :#: (TIgnore,-1))
buscarVar ((id :#: (t,f)):xs) idVariavel
    | id == idVariavel = (id :#: (t,f))
    | otherwise = buscarVar xs idVariavel

buscarFuncao :: [Funcao] -> Id -> (Tipo, Id, [Var])
buscarFuncao [] id = (TIgnore, id, [])
buscarFuncao ((id :->: (pars, tipo)):xs) idFuncao
    | id == idFuncao = (tipo, id, pars)
    | otherwise = buscarFuncao xs idFuncao

calcularFrame [] _ = []
calcularFrame ((id :#: (t, _)):vs) f
    | t == TInt || t == TString = (id :#: (t, f)) : calcularFrame vs (f+1)
    | otherwise = (id :#: (t, f)) : calcularFrame vs (f+2)

calcularFrames _ [] = ([], [])
calcularFrames fun ((id, vars, b):cs) = (newFunc : newFunTab, newCod : newCodsTab)
    where
        (t, i, parsFormais) = buscarFuncao fun id
        varsCalculadas = calcularFrame (parsFormais ++ vars) 0
        newParsFormais = take (length parsFormais) varsCalculadas
        newVars = drop (length parsFormais) varsCalculadas
        newFunc = id :->: (newParsFormais, t)
        newCod = (id, newVars, b)
        (newFunTab, newCodsTab) = calcularFrames fun cs

calcularLimitLocals [] = 0
calcularLimitLocals vars = 
    let (_ :#: (t, f)) = last vars
    in if t == TDouble then f + 2 else f + 1

novoLabel::State Int String
novoLabel = do
    n <- get
    put (n+1)
    return ("L"++show n)

genCab nome = return (".class public " ++ nome ++
                      "\n.super java/lang/Object\n\n")

genMainCab s l = return (".method public static main([Ljava/lang/String;)V" ++
                         "\n\t.limit stack " ++ show s ++
                         "\n\t.limit locals " ++ show l ++ "\n\n")

declararScanner = return ".field public static read Ljava/util/Scanner;\n\n"
construtorClasse = return (".method public <init>()V\n" ++
                            "\t.limit stack 1\n" ++
                            "\t.limit locals 1\n" ++
                            "\taload_0\n" ++
                            "\tinvokenonvirtual java/lang/Object/<init>()V\n" ++
                            "\treturn\n" ++
                            ".end method\n\n")

instanciarScanner className = return (".method static <clinit>()V\n" ++
                                    "\t.limit stack 3\n" ++
                                    "\t.limit locals 0\n" ++
                                    "\tnew java/util/Scanner\n" ++
                                    "\tdup\n" ++
                                    "\tgetstatic java/lang/System/in Ljava/io/InputStream;\n" ++
                                    "\tinvokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n" ++
                                    "\tputstatic " ++ className ++ "/read Ljava/util/Scanner;\n" ++
                                    "\treturn\n" ++
                                    ".end method\n\n")

genScanner className = do
    declarar <- declararScanner
    construir <- construtorClasse
    instanciar <- instanciarScanner className
    return (declarar ++ construir ++ instanciar)

genParametro (id :#: (t, f)) = genTipo t
genParametros pars = foldr ((++) . genParametro) [] pars

genFuncCab (id :->: (pars, t)) s l = return (".method public static " ++ id ++ "(" ++ genParametros pars ++ ")" ++ genTipo t
                                             ++ "\n\t.limit stack " ++ show s
                                             ++ "\n\t.limit locals " ++ show l ++ "\n\n")

genTipo t
    | t == TInt = "I"
    | t == TDouble = "D"
    | t == TString = "Ljava/lang/String;"
    | t == TVoid = "V"

genInt i
    | i == -1 = "\ticonst_m1\n"
    | 0 <= i && i <= 5 = "\ticonst_" ++ show i ++ "\n"
    | -128 <= i && i <= 127 = "\tbipush " ++ show i ++ "\n"
    | -32768 <= i && i <= 32767 = "\tsipush " ++ show i ++ "\n"
    | otherwise = "\tldc " ++ show i ++ "\n"

genDouble d
    | d == 0.0  = "\tdconst_0\n"
    | d == 1.0  = "\tdconst_1\n"
    | otherwise = "\tldc2_w " ++ show d ++ "\n"

genExprL c tab fun v f (Rel e) = genExprR c tab fun v f e

genExprL c tab fun v f (Not e) = genExprL c tab fun f v e

genExprL c tab fun v f (Or e1 e2) = do
    l1 <- novoLabel
    newE1 <- genExprL c tab fun v l1 e1
    newE2 <- genExprL c tab fun v f e2
    return (newE1 ++ l1 ++ ":\n" ++ newE2)

genExprL c tab fun v f (And e1 e2) = do
    l1 <- novoLabel
    newE1 <- genExprL c tab fun l1 f e1
    newE2 <- genExprL c tab fun v f e2
    return (newE1 ++ l1 ++ ":\n" ++ newE2)
-- todo

genRel t v op
    | t == TInt = "\tif_icmp" ++ r
    | t == TDouble = "\tdcmpg\n\tif" ++ r
    where
        r = op ++ " " ++ v ++ "\n"

auxGenExprR c tab fun v f e1 e2 op = do
    (t1, newE1) <- genExpr c tab fun e1
    (t2,newE2) <- genExpr c tab fun e2
    return (newE1 ++ newE2 ++ genRel t1 v op ++ "\tgoto " ++ f ++ "\n")

genExprR c tab fun v f (Req e1 e2) = auxGenExprR c tab fun v f e1 e2 "eq"
genExprR c tab fun v f (Rdif e1 e2) = auxGenExprR c tab fun v f e1 e2 "ne"
genExprR c tab fun v f (Rgt e1 e2) = auxGenExprR c tab fun v f e1 e2 "gt"
genExprR c tab fun v f (Rlt e1 e2) = auxGenExprR c tab fun v f e1 e2 "lt"
genExprR c tab fun v f (Rge e1 e2) = auxGenExprR c tab fun v f e1 e2 "ge"
genExprR c tab fun v f (Rle e1 e2) = auxGenExprR c tab fun v f e1 e2 "le"
-- todo

genOp t op = "\t" ++ map toLower (genTipo t) ++ op ++ "\n"

genExprA c tab fun e1 e2 op = do
    (t1, newE1) <- genExpr c tab fun e1
    (t2, newE2) <- genExpr c tab fun e2
    return (t1, newE1 ++ newE2 ++ genOp t1 op)

genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Const (CDouble d)) = return (TDouble, genDouble d)

genExpr c tab fun (IdVar id) = do
    let (_ :#: (t, f)) = buscarVar tab id
    return (t, loadCmd t f ++ "\n")
    where
        loadCmd TInt frame
            | 0 <= frame && frame <= 3 = "\tiload_" ++ show frame
            | otherwise = "\tiload " ++ show frame
        loadCmd TDouble frame
            | 0 <= frame && frame <= 3 = "\tdload_" ++ show frame
            | otherwise = "\tdload " ++ show frame
        loadCmd TString frame
            | 0 <= frame && frame <= 3 = "\taload_" ++ show frame
            | otherwise = "\taload " ++ show frame

genExpr c tab fun (Lit s) = return (TString, "\tldc " ++ show s ++ "\n")

genExpr c tab fun (IntDouble e) = do
    (t1, newE) <- genExpr c tab fun e
    return (TDouble, newE ++ "\ti2d\n")

genExpr c tab fun (DoubleInt e) = do
    (t1, newE) <- genExpr c tab fun e
    return (TInt, newE ++ "\td2i\n")

genExpr c tab fun (Add e1 e2) = genExprA c tab fun e1 e2 "add"
genExpr c tab fun (Sub e1 e2) = genExprA c tab fun e1 e2 "sub"
genExpr c tab fun (Mul e1 e2) = genExprA c tab fun e1 e2 "mul"
genExpr c tab fun (Div e1 e2) = genExprA c tab fun e1 e2 "div"
genExpr c tab fun (Neg e) = do
    (t, newE) <- genExpr c tab fun e
    let cmd = if t == TInt then "\tineg\n" else "\tdneg\n"
    return (t, newE ++ cmd)

genExpr c tab fun (Chamada idFuncao pars) = do
    let (t, i, parsFormais) = buscarFuncao fun idFuncao
    let tiposParsFormais = concatMap (\(_ :#: (t,_)) -> genTipo t) parsFormais
    listaPars <- mapM (genExpr c tab fun) pars
    let codPars = concatMap snd listaPars
    return (t, codPars ++ "\tinvokestatic " ++ c ++ "/" ++ idFuncao ++ "(" ++ tiposParsFormais ++ ")" ++ genTipo t ++ "\n")

genBloco c tab fun cmds = do
    lista <- mapM (genCmd c tab fun) cmds
    return (concat lista)

genCmd c tab fun (If e b1 b2) = do
    lv <- novoLabel
    lf <- novoLabel
    fim <- novoLabel
    newE <- genExprL c tab fun lv lf e
    newB1 <- genBloco c tab fun b1
    newB2 <- genBloco c tab fun b2
    return (newE ++ lv ++ ":\n" ++ newB1 ++ "\tgoto " ++ fim ++ "\n" ++ lf ++ ":\n" ++ newB2 ++ fim ++ ":\n")

genCmd c tab fun (While e b) = do
    li <- novoLabel
    lv <- novoLabel
    lf <- novoLabel
    e' <- genExprL c tab fun lv lf e
    b' <- genBloco c tab fun b
    return (li ++ ":\n" ++ e' ++ lv ++ ":\n" ++ b' ++ "\tgoto " ++ li ++ "\n" ++ lf ++ ":\n")

genCmd c tab fun (Atrib id e) = do
    (t, newE) <- genExpr c tab fun e
    let (_ :#: (_, frame)) = buscarVar tab id
    return (newE ++ cmd t frame ++ "\n")
    where
        cmd TInt frame
            | 0 <= frame && frame <= 3 = "\tistore_" ++ show frame
            | otherwise = "\tistore " ++ show frame
        cmd TDouble frame
            | 0 <= frame && frame <= 3 = "\tdstore_" ++ show frame
            | otherwise = "\tdstore " ++ show frame
        cmd TString frame
            | 0 <= frame && frame <= 3 = "\tastore_" ++ show frame
            | otherwise = "\tastore " ++ show frame

genCmd c tab fun (Leitura id) = do
    let (i :#: (t, f)) = buscarVar tab id
    return ("\tgetstatic " ++ c ++ "/read Ljava/util/Scanner;\n\tinvokevirtual java/util/Scanner/" ++ cmd t f ++ "\n")
    where
        cmd t f
            | t == TInt = "nextInt()I\n\t" ++ if 0 <= f && f <= 3 then "istore_" ++ show f else "istore " ++ show f
            | t == TDouble = "nextDouble()D\n\t" ++ if 0 <= f && f <= 3 then "dstore_" ++ show f else "dstore " ++ show f
            | otherwise = "nextLine()Ljava/lang/String;\n\t" ++ if 0 <= f && f <= 3 then "astore_" ++ show f else "astore " ++ show f

genCmd c tab fun (Imp e) = do
    (t, newE) <- genExpr c tab fun e
    return ("\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n" ++ newE ++ "\tinvokevirtual java/io/PrintStream/println" ++ cmd t ++ "\n")
    where
        cmd t
            | t == TInt = "(I)V"
            | t == TDouble = "(D)V"
            | t == TString = "(Ljava/lang/String;)V"

genCmd c tab fun (Ret Nothing) = return "\treturn\n"
genCmd c tab fun (Ret (Just e)) = do
    (t, newE) <- genExpr c tab fun e
    return (newE ++ "\t" ++ pre t ++ "return\n")
    where
        pre t
          | t == TInt = "i"
          | t == TDouble = "d"
          | otherwise = "a"

genCmd c tab fun (Proc id exprs) = do
    newExprs <- mapM (genExpr c tab fun) exprs
    let codExprs = concatMap snd newExprs
    let (t, i, parsFormais) = buscarFuncao fun id
    return (codExprs ++ "\tinvokestatic " ++ c ++ "/" ++ id ++ "(" ++ genParametros parsFormais ++ ")" ++ genTipo t ++ "\n")

initCmd :: Var -> String
initCmd (_ :#: (t, f))
    | t == TInt    = "\ticonst_0\n\t" ++ (if f <= 3 then "istore_" else "istore ") ++ show f ++ "\n"
    | t == TDouble = "\tdconst_0\n\t" ++ (if f <= 3 then "dstore_" else "dstore ") ++ show f ++ "\n"
    | otherwise    = "\taconst_null\n\t" ++ (if f <= 3 then "astore_" else "astore ") ++ show f ++ "\n"

genFuncao c fun s (id, vars, bloco) = do
    let (t, i, parsFormais) = buscarFuncao fun id
    let inits = concatMap initCmd vars
    newBloco <- genBloco c (parsFormais ++ vars) fun bloco
    return (".method public static " ++ id ++ "(" ++ genParametros parsFormais ++ ")" ++ genTipo t ++
            "\n\t.limit stack " ++ show s ++ "\n\t.limit locals " ++ show (calcularLimitLocals (parsFormais ++ vars)) ++
            "\n" ++ inits ++ newBloco ++ ".end method\n\n")

genProg nome (Prog fun codFun var m) = do
    let (newFun, newCodFun) = calcularFrames fun codFun
    let newVars = calcularFrame var 0
    cabClasse <- genCab nome
    scanner <- genScanner nome
    cabMain <- genMainCab 20 (calcularLimitLocals newVars)
    let initVarsMain = concatMap initCmd newVars
    codMain <- genBloco nome newVars newFun m
    let endMain = ".end method\n\n"
    listCodFunc <- mapM (genFuncao nome newFun 20) newCodFun
    let genCodFunc = concat listCodFunc
    return (cabClasse ++ scanner ++ genCodFunc ++ cabMain ++ initVarsMain ++ codMain ++ endMain)

gerador nome p = fst $ runState (genProg nome p) 0    

testGerador = do
    file <- readFile "teste.j--"
    let prog = P.calc (L.alexScanTokens file)
    (status, newProg) <- S.semantico prog
    if status == False then return (gerador "Teste" newProg) else return ""
