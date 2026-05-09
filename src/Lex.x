{
module Lex where 

import Token
}

%wrapper "basic"

$digit [0-9]
$nonzero [1-9]
$letter = [A-Za-z]

@sign = [\-\+]
@base = (0 | $nonzero $digit*)

@int = @sign? @base
@double = @sign? (@base \. $digit* | \. $digit+)
@id = ($letter | \_) ($letter | $digit | \_)*

tokens :-

-- Palavras Chave
<0> "if"        {\s -> IF}
<0> "else"      {\s -> ELSE}
<0> "return"    {\s -> RETURN}
<0> "print"     {\s -> PRINT}
<0> "int"       {\s -> INT}
<0> "double"    {\s -> DOUBLE}

<0> void        {\s -> VOID}
-- Números
<0> @double     {\s -> NUMDOUBLE (read s)}
<0> @int        {\s -> NUMINT (read s)}
-- Identificador de variável
<0> @id         {\s -> ID s}
-- Constante para strings

-- Operador de atribuição
<0> "="         {\s -> ASSIGN}
--Operadores Aritméticos
<0> "+"         {\s -> ADD}
<0> "-"         {\s -> SUB}
<0> "*"         {\s -> MUL}
<0> "/"         {\s -> DIV} 
-- Operadores Relacionais
<0> "=="        {\s -> EQ}
<0> "/="        {\s -> DIFF}
<0> "<"         {\s -> LT}
<0> ">"         {\s -> GT}
<0> "<="        {\s -> LE}
<0> ">="        {\s -> GE}
-- Operadores Lógicos
<0> "&&"        {\s -> AND}
<0> "||"        {\s -> OR}
<0> "!"         {\s -> NOT}
-- Chaves
<0> "{"         {\s -> LBRACES}
<0> "}"         {\s -> RBRACES}
-- Parênteses
<0> "("         {\s -> LPAR}
<0> ")"         {\s -> RPAR}
-- Símbolos Especiais
<0> ","         {\s -> COMMA}
<0> ";"         {\s -> CMDEND}

{
testLex = do s <- getLine -- getContents
             print (alexScanTokens s)
}