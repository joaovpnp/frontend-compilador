{
module Parser where

import Token
import AST
import qualified Lex as L

}


%name calc
%tokentype { Token }
%error { parseError }
%token
  if      {IF}                           -- if
  else    {ELSE}                         -- else
  while   {WHILE}                        -- while
  return  {RETURN}                       -- return
  read    {READ}                         -- read
  print   {PRINT}                        -- print
  int     {INT}                          -- int
  double  {DOUBLE}                       -- double
  string  {STRING}                       -- string
  void    {VOID}                         -- void
  NumInt  {NUMINT $$}
  NumDoub {NUMDOUBLE $$}
  Id      {ID $$}
  Literal {LITERAL $$}
  '='     {ASSIGN}
  '+'     {ADD}
  '-'     {SUB}
  '*'     {MUL}
  '/'     {DIV}
  '=='    {TEQ}
  '/='    {DIFF}
  '<'     {TLT}
  '>'     {TGT}
  '<='    {LE}
  '>='    {GE}
  '&&'    {AND}
  '||'    {OR}
  '!'     {NOT}
  '{'     {LBRACES}
  '}'     {RBRACES}
  '('     {LPAR}
  ')'     {RPAR}
  ','     {COMMA}
  ';'     {CMDEND}

%%

Programa : ListaFuncoes BlocoPrincipal                           {Prog (separarAssinatura $1) (separarBloco $1) (fst $2) (snd $2)}
         | BlocoPrincipal                                        {Prog [] [] (fst $1) (snd $1)}

ListaFuncoes : ListaFuncoes Funcao                               {$1 ++ $2}
             | Funcao                                            {$1}

Funcao : TipoRet Id '(' ParamFormais ')' BlocoPrincipal          {[($2 :->: (getVarsFuncao $4 $6, $1), snd $6)]}
       | TipoRet Id '('')' BlocoPrincipal                        {[($2 :->: (fst $5, $1), snd $5)]}

Tipo : int                                                       {TInt}
     | double                                                    {TDouble}
     | string                                                    {TString}

TipoRet : Tipo                                                   {$1}
        | void                                                   {TVoid}

ParamFormais : ParamFormais ',' ParamFormal                      {$1 ++ $3}
             | ParamFormal                                       {$1}

ParamFormal : Tipo Id                                            {[($1, $2)]}

BlocoPrincipal : '{' Declaracoes ListaCMD '}'                    {($2, $3)}
               | '{' ListaCMD '}'                                {([], $2)}

Declaracoes : Declaracoes Declaracao                             {$1 ++ $2}
            | Declaracao                                         {$1}

Declaracao : Tipo ListaId ';'                                    {map (\x -> x :#: ($1, 0)) $2}

ListaId : ListaId ',' Id                                         {$1 ++ [$3]}
        | Id                                                     {[$1]}

Bloco : '{' ListaCMD '}'                                         {$2}

ListaCMD : ListaCMD Comando                                      {$1 ++ $2}
         | Comando                                               {$1}

Comando : CmdIf                                                  {[$1]}
        | CmdWhile                                               {[$1]}
        | CmdAtrib                                               {[$1]}
        | CmdEscrita                                             {[$1]}
        | CmdLeitura                                             {[$1]}
        | Retorno                                                {[$1]}
        | ChamadaProc                                            {[$1]}

CmdIf : if '(' ExprLogica ')' Bloco                              {If $3 $5 []}
      | if '(' ExprLogica ')' Bloco else Bloco                   {If $3 $5 $7}

CmdWhile : while '(' ExprLogica ')' Bloco                        {While $3 $5}

CmdAtrib : Id '=' ExprAritmetica ';'                             {Atrib $1 $3}
         | Id '=' Literal                                        {Atrib $1 (Lit $3)}

CmdEscrita : print '(' ExprAritmetica ')' ';'                    {Imp $3}
           | print '(' Literal ')' ';'                           {Imp (Lit $3)}

CmdLeitura : read '(' Id ')' ';'                                 {Leitura $3}

Retorno : return ExprAritmetica ';'                              {Ret (Just $2)}
        | return Literal ';'                                     {Ret (Just (Lit $2))}
        | return ';'                                             {Ret Nothing}

ChamadaProc : ChamadaFuncao ';'                                  {(\(Chamada nome exprs) -> Proc nome exprs) $1}

ChamadaFuncao : Id '(' ParamReais ')'                            {Chamada $1 $3}
              | Id '('')'                                        {Chamada $1 []}

ParamReais : ParamReais ',' ExprAritmetica                       {$1 ++ [$3]}
           | ParamReais ',' Literal                              {$1 ++ [Lit $3]}
           | ExprAritmetica                                      {[$1]}
           | Literal                                             {[Lit $1]}

ExprAritmetica : ExprAritmetica '+' Term                         {Add $1 $3}
               | ExprAritmetica '-' Term                         {Sub $1 $3}
               | Term                                            {$1}

Term : Term '*' Factor                                           {Mul $1 $3}
     | Term '/' Factor                                           {Div $1 $3}
     | Factor                                                    {$1}

Factor : NumInt                                                  {Const (CInt $1)}
       | NumDoub                                                 {Const (CDouble $1)}
       | Id                                                      {IdVar $1}
       | ChamadaFuncao                                           {$1}
       | '(' ExprAritmetica ')'                                  {$2}
       | '-' Factor                                              {Neg $2}

ExprRelacional : ExprAritmetica '==' ExprAritmetica              {Req $1 $3}
               | ExprAritmetica '/=' ExprAritmetica              {Rdif $1 $3}
               | ExprAritmetica '<' ExprAritmetica               {Rlt $1 $3}
               | ExprAritmetica '>' ExprAritmetica               {Rgt $1 $3}
               | ExprAritmetica '<=' ExprAritmetica              {Rle $1 $3}
               | ExprAritmetica '>=' ExprAritmetica              {Rge $1 $3}

ExprLogica : ExprLogica '||' TermLogico                          {Or $1 $3}
           | ExprLogica '&&' TermLogico                          {And $1 $3}
           | TermLogico                                          {$1}

TermLogico : '!' '(' ExprLogica ')'                              {Not $3}
           | '(' ExprLogica ')'                                  {$2}
           | ExprRelacional                                      {Rel $1}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

criarVars [] = []
criarVars ((tipo, nome):pars) = (nome :#: (tipo, 0)) : criarVars pars

getVarsFuncao [] bloco = fst bloco
getVarsFuncao pars bloco = (criarVars pars) ++ (fst bloco)

separarAssinatura xs = (fst . unzip) xs

separarBloco [] = []
separarBloco ((id :->: (vars, tipo), bloco):xs) = (id, vars, bloco) : separarBloco xs

main = do prog <- readFile "teste.j--"
          let a = calc (L.alexScanTokens prog)
          putStr (printProg a)
}
