module Main (main) where

import Jogo ( criarJogo, jogarJogo, solicitarJogadores )
import Control.Monad.Trans.State ( evalStateT )

main :: IO ()
main = do
    painel1 <- readFile "./menu/painel1.txt"
    putStrLn painel1
    nomesDosJogadores <- solicitarJogadores
    jogo <- criarJogo nomesDosJogadores
    evalStateT jogarJogo jogo
    return ()