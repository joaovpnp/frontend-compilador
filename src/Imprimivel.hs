module Imprimivel where

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
    fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
    pure a = Result (False, "", a)
    Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

instance Monad Result where 
    -- return a = Result (False, "", a)
    Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')
  
errorMsg s = Result (True, "ERRO: "++s++"\n\n", ())

warningMsg s = Result (False, "ADVERTENCIA: "++s++"\n\n", ())

class Imprimivel a where
    formatar :: a -> String

-- Função genérica para emitir erro com base na AST
erroDeTipo :: (Imprimivel a1, Imprimivel a2) => String -> String -> a1 -> a2 -> Result ()
erroDeTipo mensagem nomeFuncao cmd elemento = errorMsg ("na funcao " ++ nomeFuncao ++ "\n\t-> no comando: " ++ formatar cmd ++ "\n\t\t-> " ++ mensagem ++ " na expressao: " ++ formatar elemento)

-- Função genérica para emitir aviso
avisoDeCast :: (Imprimivel a1, Imprimivel a2) => String -> String -> a1 -> a2 -> Result ()
avisoDeCast mensagem nomeFuncao cmd elemento = warningMsg ("na funcao " ++ nomeFuncao ++ "\n\t-> no comando: " ++ formatar cmd ++ "\n\t\t-> " ++ mensagem ++ " na expressao: " ++ formatar elemento)

