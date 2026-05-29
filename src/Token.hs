{--

Este módulo visa definir todos os tokens da linguagem que serão retornados pelo Lexer para o Parser.

--}

module Token where

data Token
    -- Palavras-chave
    = IF                           -- if
    | ELSE                         -- else
    | WHILE                        -- while
    | RETURN                       -- return
    | READ                         -- read
    | PRINT                        -- print
    | INT                          -- int
    | DOUBLE                       -- double
    | STRING                       -- string
    | VOID                         -- void
    -- Números
    | NUMINT Int
    | NUMDOUBLE Double
    -- Identificador de variável
    | ID String
    -- Constante para strings
    | LITERAL String
    -- Operador de atribuicao
    | ASSIGN                       -- =
    -- Operadores aritméticos
    | ADD                          -- +
    | SUB                          -- -
    | MUL                          -- *
    | DIV                          -- /
    -- Operadores relacionais
    | TEQ                           -- ==
    | DIFF                         -- /=
    | TLT                           -- <
    | TGT                           -- >
    | LE                           -- <=
    | GE                           -- >=
    -- Operadores lógicos
    | AND                          -- &&
    | OR                           -- ||
    | NOT                          -- !
    -- Chaves
    | LBRACES                      -- {
    | RBRACES                      -- }
    -- Parênteses
    | LPAR                         -- (
    | RPAR                         -- )
    -- Símbolos especiais
    | COMMA                        -- ,
    | CMDEND                       -- ;
    -- Token para o analisador semântico
    | PROC
    deriving (Show,Eq)