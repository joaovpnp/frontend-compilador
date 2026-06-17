import Imprimivel
import qualified Lex as L
import qualified Parser as P
import qualified Semantico as S
import qualified Gerador as G

compilar = do
    file <- readFile "teste.j--"
    let programa = P.calc (L.alexScanTokens file)
    let Result (status, mensagem, progVerificado) = S.verificarPrograma programa
    let nomeClasse = "Teste"
    if status == True then putStrLn ("\nERRO DE COMPILACAO\n\n" ++ mensagem ++ "\n")
    else do
        putStrLn (mensagem ++ "\n")
        let bytecode = G.gerador nomeClasse progVerificado
        writeFile ("../" ++ nomeClasse ++ ".j") bytecode
        putStrLn ("\nCompilacao concluida! Arquivo " ++ nomeClasse ++ ".j gerado com sucesso.\n")

