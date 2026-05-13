module AST where

type Id = String

data Tipo = TDouble | TInt | TString | TVoid deriving (Show, Eq)

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

printBloco :: [Comando] -> String
printBloco [] = ""
printBloco (c:cs) = show c ++ "\n" ++ printBloco cs

printVars :: [Var] -> String
printVars [] = ""
printVars [nome :#: (tipo, frame)] = 
    nome ++ " :: " ++ show tipo ++ " (frame " ++ show frame ++ ")\n"
printVars ((nome :#: (tipo, frame)):vs) = 
    nome ++ " :: " ++ show tipo ++ " (frame " ++ show frame ++ "),\n" ++ 
    printVars vs

printAssFuncoes :: [Funcao] -> String
printAssFuncoes [] = ""
printAssFuncoes ((nome :->: (vars, tipo)):fs) = 
    show tipo ++ " " ++ nome ++ ", variaveis:\n" ++ 
    printVars vars ++ "\n" ++ 
    printAssFuncoes fs

printCodFuncoes :: [(Id, [Var], Bloco)] -> String
printCodFuncoes [] = ""
printCodFuncoes ((nome, vars, bloco):cs) = 
    nome ++ " {\n\n" ++ 
    printBloco bloco ++ 
    "}\n\n" ++ 
    printCodFuncoes cs

printProg :: Programa -> String
printProg (Prog assFuncoes codFuncoes varGlobais codPrincipal) = 
    "Programa\n\n" ++
    "Assinaturas das Funcoes\n\n" ++ 
    printAssFuncoes assFuncoes ++ "\n" ++
    "Codigo das Funcoes\n\n" ++ 
    printCodFuncoes codFuncoes ++ 
    "Variaveis Globais\n\n" ++ 
    printVars varGlobais ++ "\n\n" ++
    "Main\n\n" ++ 
    printBloco codPrincipal