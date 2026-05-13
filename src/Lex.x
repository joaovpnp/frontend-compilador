{
module Lex where 

import Token
}

%wrapper "basic"

$digit = [0-9]
$nonzero = [1-9]
$letter = [A-Za-z]

-- @sign = [\-\+]
@base = (0 | $nonzero $digit*)

@int = @base
@double = (@base \. $digit* | \. $digit+)
@id = ($letter | \_) ($letter | $digit | \_)*
@string = \" [^\"]* \"

tokens :-

-- Whitespace 
<0> $white+             ;
-- Palavras Chave
<0> "if"                {\s -> IF}
<0> "else"              {\s -> ELSE}
<0> "while"             {\s -> WHILE}
<0> "return"            {\s -> RETURN}
<0> "read"              {\s -> READ}
<0> "print"             {\s -> PRINT}
<0> "int"               {\s -> INT}
<0> "double"            {\s -> DOUBLE}
<0> "string"            {\s -> STRING}
<0> "void"              {\s -> VOID}
-- Números
<0> 0 $digit+    {\s -> error ("Erro léxico: número iniciando com zero: " ++ s)}
<0> @double             {\s -> NUMDOUBLE (read s)}
<0> @int                {\s -> NUMINT (read s)}
-- Identificador de variável
<0> @id                 {\s -> ID s}
-- Constante para strings
<0> @string             {\s -> LITERAL (init (tail s))}
-- Operador de atribuição
<0> "="                 {\s -> ASSIGN}
--Operadores Aritméticos
<0> "+"                 {\s -> ADD}
<0> "-"                 {\s -> SUB}
<0> "*"                 {\s -> MUL}
<0> "/"                 {\s -> DIV} 
-- Operadores Relacionais
<0> "=="                {\s -> TEQ}
<0> "/="                {\s -> DIFF}
<0> "<"                 {\s -> TLT}
<0> ">"                 {\s -> TGT}
<0> "<="                {\s -> LE}
<0> ">="                {\s -> GE}
-- Operadores Lógicos
<0> "&&"                {\s -> AND}
<0> "||"                {\s -> OR}
<0> "!"                 {\s -> NOT}
-- Chaves
<0> "{"                 {\s -> LBRACES}
<0> "}"                 {\s -> RBRACES}
-- Parênteses
<0> "("                 {\s -> LPAR}
<0> ")"                 {\s -> RPAR}
-- Símbolos Especiais
<0> ","                 {\s -> COMMA}
<0> ";"                 {\s -> CMDEND}

{
testLex = do 
        arqv <- readFile "teste.j--"
        print (alexScanTokens arqv)
}