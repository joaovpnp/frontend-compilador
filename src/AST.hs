module AST where

import Data.List (intercalate)
import Imprimivel
import Token

type Id = String

data Tipo = TDouble | TInt | TString | TVoid | TIgnore deriving (Show, Eq)

data TCons = CDouble Double | CInt Int deriving Show

data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr
            | Div Expr Expr | Neg Expr | Const TCons
            | IdVar String | Chamada Id [Expr] | Lit String
            | IntDouble Expr | DoubleInt Expr
            deriving Show

data ExprR = Req Expr Expr
            | Rdif Expr Expr | Rlt Expr Expr
            | Rgt Expr Expr | Rle Expr Expr | Rge Expr Expr
            deriving Show

data ExprL = And ExprL ExprL | Or ExprL ExprL | Not ExprL | Rel ExprR deriving Show

data Var = Id :#: (Tipo, Int) deriving Show

data Funcao = Id :->: ([Var], Tipo) deriving Show

data Programa = Prog [Funcao] [(Id, [Var], Bloco)] [Var] Bloco deriving Show

type Bloco = [Comando]

data Comando = If ExprL Bloco Bloco
            | While ExprL Bloco
            | Atrib Id Expr
            | Leitura Id
            | Imp Expr
            | Ret (Maybe Expr)
            | Proc Id [Expr]
            deriving Show

-- Funções para melhorar legibilidade da AST

printTipo :: Tipo -> String
printTipo t
    | t == TDouble = "double"
    | t == TInt = "int"
    | t == TString = "string"
    | otherwise = "void"

printConst :: TCons -> String
printConst (CDouble c) = show c
printConst (CInt c) = show c

printExpr :: Expr -> String
printExpr (IdVar id) = id
printExpr (Lit literal) = "\"" ++ literal ++ "\""
printExpr (Const c) = printConst c
printExpr (IntDouble e) = "(double)" ++ printExpr e
printExpr (DoubleInt e) = "(int)" ++ printExpr e
printExpr (Neg e) = "-(" ++ printExpr e ++ ")"
printExpr (Add e1 e2) = "(" ++ printExpr e1 ++ ") + (" ++ printExpr e2 ++ ")"
printExpr (Sub e1 e2) = "(" ++ printExpr e1 ++ ") - (" ++ printExpr e2 ++ ")"
printExpr (Mul e1 e2) = "(" ++ printExpr e1 ++ ") * (" ++ printExpr e2 ++ ")"
printExpr (Div e1 e2) = "(" ++ printExpr e1 ++ ") / (" ++ printExpr e2 ++ ")"
printExpr (Chamada id exps) = id ++ "(" ++ intercalate ", " (map printExpr exps) ++ ")"

printExprRel :: ExprR -> String
printExprRel (Req e1 e2) = printExpr e1 ++ " == " ++ printExpr e2
printExprRel (Rdif e1 e2) = printExpr e1 ++ " != " ++ printExpr e2
printExprRel (Rlt e1 e2) = printExpr e1 ++ " < " ++ printExpr e2
printExprRel (Rgt e1 e2) = printExpr e1 ++ " > " ++ printExpr e2
printExprRel (Rle e1 e2) = printExpr e1 ++ " <= " ++ printExpr e2
printExprRel (Rge e1 e2) = printExpr e1 ++ " >= " ++ printExpr e2

printExprLog :: ExprL -> String
printExprLog (And e1 e2) = "(" ++ printExprLog e1 ++ ") && (" ++ printExprLog e2 ++ ")"
printExprLog (Or e1 e2) = "(" ++ printExprLog e1 ++ ") || (" ++ printExprLog e2 ++ ")"
printExprLog (Not e1) = "!(" ++ printExprLog e1 ++ ")"
printExprLog (Rel r) = printExprRel r

printComando :: Comando -> String
printComando (If el b1 []) = "if (" ++ printExprLog el ++ ") {\n" ++ printBloco b1 ++ "}"
printComando (If el b1 b2) = "if (" ++ printExprLog el ++ ") {\n" ++ printBloco b1 ++ "} else {\n" ++ printBloco b2 ++ "}"
printComando (While el b) = "while (" ++ printExprLog el ++ ") {\n" ++ printBloco b ++ "}"
printComando (Atrib id e) = id ++ " = " ++ printExpr e
printComando (Leitura id) = "read(" ++ id ++ ")"
printComando (Imp e) = "print(" ++ printExpr e ++ ")"
printComando (Ret Nothing) = "return"
printComando (Ret (Just e)) = "return (" ++ printExpr e ++ ")"
printComando (Proc id exps) = printExpr (Chamada id exps)

printBloco :: [Comando] -> String
printBloco [] = ""
printBloco ((If el b1 b2):cs) = printComando (If el b1 b2) ++ "\n" ++ printBloco cs
printBloco ((While el b):cs) = printComando (While el b) ++ "\n" ++ printBloco cs
printBloco (c:cs) = printComando c ++ ";\n" ++ printBloco cs

printVar :: Var -> String
printVar (nome :#: (tipo, frame)) = printTipo tipo ++ " " ++ nome ++ " (frame " ++ show frame ++ ")"

printVars :: [Var] -> String
printVars [] = ""
printVars [v] = printVar v ++ ";\n"
printVars (v:vs) = printVar v ++ ";\n" ++ printVars vs

printPars :: [Var] -> [String]
printPars [] = []
printPars (v:vs) = [printVar v] ++ printPars vs

printAssinaturaFuncao :: Funcao -> String
printAssinaturaFuncao (nome :->: (vars, tipo)) = 
    printTipo tipo ++ " " ++ nome ++ "(" ++ (intercalate ", " (printPars vars)) ++ ")"

printCodFuncoes :: [Funcao] -> [(Id, [Var], Bloco)] -> String
printCodFuncoes [] [] = ""
printCodFuncoes (f:fs) ((nome, vars, bloco):cs) = 
    printAssinaturaFuncao f ++ " {\n" ++
    printVars vars ++
    printBloco bloco ++ "\n}\n\n" ++ printCodFuncoes fs cs

printProg :: Programa -> String
printProg (Prog assFuncoes codFuncoes varGlobais codPrincipal) = 
    "Programa\n\n" ++
    printCodFuncoes assFuncoes codFuncoes ++
    "Main() {\n" ++
    printVars varGlobais ++
    printBloco codPrincipal ++
    "\n}\n"

-- Utilizando o polimorfismo da classe Imprimivel

instance Imprimivel Expr where
    formatar = printExpr

instance Imprimivel ExprR where
    formatar = printExprRel

instance Imprimivel ExprL where
    formatar = printExprLog

instance Imprimivel Comando where
    formatar = printComando

instance Imprimivel Funcao where
    formatar = printAssinaturaFuncao

-- funcoes para construir expressoes a partir de tokens dados
-- é necessário uma função para cada tipo: Expr, ExprR, etc.

constrExprA token e1 e2
    | token == ADD = Add e1 e2
    | token == SUB = Sub e1 e2
    | token == MUL = Mul e1 e2
    | token == DIV = Div e1 e2

constrExprR token e1 e2
    | token == TEQ = Req e1 e2
    | token == DIFF = Rdif e1 e2
    | token == TLT = Rlt e1 e2
    | token == TGT = Rgt e1 e2
    | token == LE = Rle e1 e2
    | token == GE = Rge e1 e2