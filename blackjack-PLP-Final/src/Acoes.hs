module Acoes (
    turnoDoJogador,
    comprarCarta
) where

import Tipos (Dinheiro, Mao, Jogo (..), JogoT, Jogador (..))
import RegrasDoBlackjack (valorDaMao)
import Control.Monad.Trans.State ( get, put, runState, State )
import Baralho (Carta, embaralhar, valorDaCarta, cartaASCII)
import Control.Monad.IO.Class (liftIO)

data Acao =
    Hit
    | Stand
    | Double
    | Split
    deriving (Show, Eq)

-- Traduz a entrada do jogador em ações válidas
getAcaoValida :: (Mao, Dinheiro) -> Dinheiro -> IO Acao
getAcaoValida (m, d) br = do
    (prompt, acoes) <- promptDeTurno (m, d) br
    putStrLn prompt
    acao <- getLine
    let acaoDoTurno "H" = return Hit
        acaoDoTurno "S" = return Stand
        acaoDoTurno "D" = return Double
        acaoDoTurno "P" = return Split
        acaoDoTurno _ = do
            putStrLn "Ação inválida. Tente novamente."
            getAcaoValida (m, d) br
    acaoValida <- acaoDoTurno acao
    if acaoValida `elem` acoes
        then return acaoValida
        else do
            putStrLn "Ação inválida. Tente novamente."
            getAcaoValida (m, d) br

-- Processa cada turno
promptDeTurno :: (Mao, Dinheiro) -> Dinheiro -> IO (String, [Acao])
promptDeTurno (m, d) br =
    let
        prompt = if br >= d && length m == 2
                    then if valorDaCarta (head m) == valorDaCarta (head (tail m))
                        then "O que você gostaria de fazer? " ++ "\x1b[33m" ++ "(H)it, (S)tand, (D)ouble, S(P)lit" ++ "\x1b[0m"
                        else "O que você gostaria de fazer?  " ++ "\x1b[33m" ++ "(H)it, (S)tand, (D)ouble" ++ "\x1b[0m"
                    else "O que você gostaria de fazer?  " ++ "\x1b[33m" ++ "(H)it, (S)tand" ++ "\x1b[0m"
        acoes = if br >= d && length m == 2
                    then if valorDaCarta (head m) == valorDaCarta (head (tail m))
                        then [Hit, Stand, Double, Split]
                        else [Hit, Stand, Double]
                    else [Hit, Stand]
    in
        return (prompt, acoes)

-- Processa o turno de um jogador
-- Para cada mao ativa, a mao toma acoes até ser movida para
-- as maos jogadas do jogador
-- Isso é necessário para a lógica correta de Dividir e inserir uma nova mao na ordem
turnoDoJogador :: Jogador -> JogoT IO Jogador
turnoDoJogador j = do
    let maosA = maosAtivas j
    let maosJ = maosJogadas j
    let br = banca j
    case maosA of
        [] -> return j
        ((m, d):ms) -> do
            _ <- liftIO $ putStrLn $ "É a vez de " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ "."
            _ <- liftIO $ putStrLn $ "Mão: \n" ++ cartaASCII m ++  "Valor: " ++ show (valorDaMao m)
            _ <- liftIO $ putStrLn $ "Aposta: " ++ show d
            acao <- liftIO $ getAcaoValida (m,d) br
            case acao of
                Stand -> do
                    _ <- liftIO $ putStrLn $ "O jogador " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ " para com a mão: \n" ++ cartaASCII m ++  "Valor: " ++ show (valorDaMao m)
                    (maoJogada, novoEstado) <- runState (executarAcao acao (m,d)) <$> get
                    put novoEstado
                    let novoJogador = j { maosJogadas = maosJ ++ maoJogada, maosAtivas = ms }
                    turnoDoJogador novoJogador
                Double -> do
                    (maoJogada, novoEstado) <- runState (executarAcao acao (m,d)) <$> get
                    put novoEstado
                    _ <- liftIO $ putStrLn $ "O jogador " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ " dobra a mão para: \n" ++ cartaASCII (fst $ head maoJogada) ++  " Valor: " ++ show (valorDaMao (fst $ head maoJogada))
                    let novoJogador = j { maosJogadas = maosJ ++ maoJogada, maosAtivas = ms, banca = br - d }
                    turnoDoJogador novoJogador
                Hit -> do
                    _ <- liftIO $ putStrLn $ "O jogador " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ " compra uma carta!"
                    (maoAtualizada, novoEstado) <- runState (executarAcao acao (m,d)) <$> get
                    put novoEstado
                    let maoAtual = fst $ head maoAtualizada
                    if valorDaMao maoAtual > 21
                        then do
                            _ <- liftIO $ putStrLn $ "\x1b[31m" ++ "Mão estourada! " ++ "\x1b[0m\n" ++ cartaASCII maoAtual ++ " Valor: " ++ show (valorDaMao maoAtual)
                            let novoJogador = j { maosJogadas = maosJ ++ maoAtualizada, maosAtivas = ms }
                            turnoDoJogador novoJogador
                        else do
                            let novoJogador = j { maosJogadas = maosJ, maosAtivas = maoAtualizada ++ ms }
                            turnoDoJogador novoJogador
                Split -> do
                    _ <- liftIO $ putStrLn $ "O jogador " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ " divide " ++ show (valorDaMao [head m])
                    (novaMao, novoEstado) <- runState (executarAcao acao (m,d)) <$> get
                    put novoEstado
                    let novoJogador = j { maosAtivas = novaMao ++ ms, banca = br - d }
                    turnoDoJogador novoJogador


-- Executa uma acao e faz a coisa certa com a mao dada a acao
executarAcao :: Acao -> (Mao, Dinheiro) -> State Jogo [(Mao, Dinheiro)]
executarAcao Stand (m, d) = return [(m, d)]
executarAcao Hit (m, d) = do
    novaCarta <- comprarCarta
    return [(m ++ [novaCarta], d)]
executarAcao Double (m, d) = if length m == 2
                                then do
                                    novaCarta <- comprarCarta
                                    return [(novaCarta : m, d * 2)]
                                else do
                                    return [(m,d)]
executarAcao Split (m, d) = if length m == 2 && valorDaCarta (head m) == valorDaCarta (head (tail m))
                                then do
                                    novaCarta1 <- comprarCarta
                                    novaCarta2 <- comprarCarta
                                    return [(novaCarta1 : [head m], d), (novaCarta2 : [head (tail m)], d)]
                                else do
                                    return [(m,d)]

-- Método de compra com estado. Se o baralho estiver vazio, ele embaralha o monte de descarte para comprar uma nova carta
-- Quando o monte de descarte é embaralhado, ele muda a penetracao para indicar ao jogo
-- Para reembaralhar e inserir uma carta de corte no final da rodada
comprarCarta :: State Jogo Carta
comprarCarta = do
    gs <- get
    case baralho gs of
        [] -> do
            let (novoBaralho, novoGen) = embaralhar (descarte gs) (gen gs)
            put $ gs { baralho = novoBaralho, descarte = [], gen = novoGen, penetracao = 2 * length novoBaralho }
            comprarCarta
        (c:cs) -> do
            put $ gs { baralho = cs }
            return c