module Seguro (
    processarSeguro
) where

import Tipos (Jogador (..), Jogo(..), JogoT)
import RegrasDoBlackjack ( dealerBlackjack )
import Control.Monad.Trans.State ( get, put )
import Control.Monad ( forM )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

-- A aposta máxima de seguro é a metade da soma de todas as apostas ativas
calcularMaximoSeguro :: Jogador -> Int
calcularMaximoSeguro jogador = min (banca jogador) (div (sum $ map snd $ maosAtivas jogador) 2)

-- Solicitar uma aposta de seguro válida, se nenhuma aposta ou aposta inválida for inserida, retorne 0
obterApostaSeguroValida :: Int -> IO Int
obterApostaSeguroValida apostaMaxima = do
    putStrLn "Gostaria de comprar seguro? (S/N)"
    resposta <- getLine
    case resposta of
        "S" -> do
            putStrLn $ "Você pode apostar até " ++ show apostaMaxima ++ "\nQuanto você gostaria de apostar?"
            aposta <- read <$> getLine
            if aposta > apostaMaxima
                then do
                    putStrLn $ "Você não pode apostar mais do que " ++ show apostaMaxima
                    obterApostaSeguroValida apostaMaxima
                else return $ max aposta 0
        "N" -> return 0
        _ -> do
            putStrLn "Resposta inválida. Tente novamente."
            obterApostaSeguroValida apostaMaxima

-- Lógica para perguntar a cada jogador por uma aposta de seguro e processar a mão do dealer
-- se o dealer tiver um blackjack, o seguro paga 2:1, caso contrário, todas as apostas de seguro são perdidas
processarSeguro :: JogoT IO ()
processarSeguro = do
    gs<- get
    let ps = jogadores gs
    let d = dealer gs
    novosJogadores <- forM ps $ \jogador -> do
        let apostaMaxima = calcularMaximoSeguro jogador
        if apostaMaxima > 0
            then do
                aposta <- liftIO $ obterApostaSeguroValida apostaMaxima
                return jogador { seguro = aposta, banca = banca jogador - aposta }
            else return jogador
    if dealerBlackjack d 
        then do
            _ <- liftIO $ putStrLn ("O " ++ "\x1b[36m" ++ "dealer " ++ "\x1b[0m" ++ "tem um blackjack. O seguro paga 2:1.")
            let novosJogadores2 = map (\jogador -> jogador { banca = banca jogador + 3 * seguro jogador, seguro = 0 }) novosJogadores
            put $ gs { jogadores = novosJogadores2 }
        else do
            _ <- liftIO $ putStrLn ("O " ++ "\x1b[36m" ++ "dealer " ++ "\x1b[0m" ++ "não tem um blackjack. O seguro é perdido.")
            let novosJogadores2 = map (\jogador -> jogador { banca = banca jogador, seguro = 0 }) novosJogadores
            put $ gs { jogadores = novosJogadores2 }