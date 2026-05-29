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
  
errorMsg s = Result (True, "ERRO: "++s++"\n", ())

warningMsg s = Result (False, "ADVERTENCIA: "++s++"\n", ())

class Imprimivel a where
    formatar :: a -> String

-- Função genérica para emitir erro com base na AST
erroDeTipo :: (Imprimivel a) => String -> a -> Result ()
erroDeTipo mensagem elemento = errorMsg (mensagem ++ " na expressao: " ++ formatar elemento)

-- Função genérica para emitir aviso
avisoDeCast :: (Imprimivel a) => String -> a -> Result ()
avisoDeCast mensagem elemento = warningMsg (mensagem ++ " na expressao: " ++ formatar elemento)
