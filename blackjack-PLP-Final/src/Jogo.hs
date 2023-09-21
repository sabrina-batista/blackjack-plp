module Jogo (
    jogarJogo,
    criarJogo,
    solicitarJogadores
) where

import Baralho ( gerarBaralhos, embaralhar, valorDaCarta )
import Tipos
    ( JogoT,
      Dealer(Dealer, maoOculta, nomeDoDealer, mao),
      Jogador(Jogador, maosAtivas, maosJogadas, seguro, banca,
             nomeDoJogador),
      Mao,
      Dinheiro,
      Jogo(..) )
import Apostas ( getApostas )
import System.Random ( initStdGen, Random(randomR) )
import Acoes ( turnoDoJogador, comprarCarta )
import RegrasDoBlackjack ( valorDaMao, dealerBlackjack, isBlackjack, bjPay )
import Abertura ( distribuirMaosIniciais )
import Seguro ( processarSeguro )
import Control.Monad.Trans.State ( get, put, runState)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM, filterM, replicateM)
import Text.Read (readMaybe)

-- Calcula a posiçao para a carta de corte
-- Se o baralho ficar menor do que a posiçao da carta de corte, o baralho é
-- embaralhado no final da rodada
cartaDeCorte :: Monad m => JogoT m Int
cartaDeCorte = do
    gs <- get
    let tamanhoBaralho = length (baralho gs)
    let (índice, gen') = randomR ((0.35 * fromIntegral tamanhoBaralho) :: Double
                                    ,(0.45 * fromIntegral tamanhoBaralho) :: Double) (gen gs)
    let posicao = ceiling índice
    let gs' = gs { penetracao = posicao, gen = gen' }
    put gs'
    return posicao

-- Loop principal do jogo
-- Continua o jogo até que todos os jogadores estejam falidos
jogarJogo :: JogoT IO ()
jogarJogo = do
    gs <- get
    let js = jogadores gs
    jogadoresComApostas <- liftIO $ getApostas js
    put $ gs { jogadores = jogadoresComApostas }
    (_, gs2) <- runState distribuirMaosIniciais <$> get
    put gs2
    let js2 = jogadores gs2
    let d = dealer gs2
    _ <- liftIO $ putStrLn ""
    liftIO $ print d
    if valorDaCarta (head (mao d)) == 1
        then do
            processarSeguro
            if dealerBlackjack d
                then do
                    processarBlackjackDoDealer
                    gs3 <- get
                    put $ gs3 {dealer = d { mao = mao d ++ maoOculta d, maoOculta = [] }}
                else do
                    novosJogadores <- forM js2 $ \jogador -> do
                        _ <- liftIO $ putStrLn ""
                        turnoDoJogador jogador
                    gs3 <- get
                    put $ gs3 { jogadores = novosJogadores, dealer = d { mao = mao d ++ maoOculta d, maoOculta = [] } }
                    _ <- liftIO $ putStrLn ""
                    turnoDoDealer
        else do
            novosJogadores <- forM js2 $ \jogador -> do
                _ <- liftIO $ putStrLn ""
                turnoDoJogador jogador
            gs3 <- get
            put $ gs3 { jogadores = novosJogadores, dealer = d { mao = mao d ++ maoOculta d, maoOculta = [] } }
            _ <- liftIO $ putStrLn ""
            turnoDoDealer
    calcularPagamentos
    limparMaos
    limparJogadores
    gs4 <- get
    if not (null (jogadores gs4))
        then do
            _ <- liftIO $ putStrLn "Próxima rodada! \n\n"
            jogarJogo
        else do
            _ <- liftIO $ putStrLn "Nenhum jogador possui saldo para apostas. Fim de jogo"
            return ()

-- Calcula os pagamentos para cada jogador e atualiza suas bancas
calcularPagamentos :: JogoT IO ()
calcularPagamentos = do
    gs <- get
    let js = jogadores gs
        d = dealer gs
    novosJogadores <- forM js $ \jogador -> do
        let bancaAtual = banca jogador
        _ <- liftIO $ putStrLn $ "\nMãos do jogador " ++ "\x1b[34m" ++ nomeDoJogador jogador ++ "\x1b[0m" ++ " :"
        pagamentos <- liftIO $ calcularPagamentosDasMaos (mao d) (maosJogadas jogador)
        let ganhos = sum pagamentos
            maosResetadas = [(m, 0) | (m, _) <- maosJogadas jogador]
        _ <- liftIO $ putStrLn $ "Jogador " ++ "\x1b[34m" ++ nomeDoJogador jogador ++ "\x1b[0m" ++ " agora tem " ++ show (bancaAtual + ganhos)
        return $ jogador { banca = bancaAtual + ganhos, maosJogadas = maosResetadas }
    put $ gs { jogadores = novosJogadores }

calcularPagamentosDasMaos :: Mao -> [(Mao, Dinheiro)] -> IO [Dinheiro]
calcularPagamentosDasMaos d = mapM (calcularPagamento d)

-- Calcula e mostra o resultado de cada mao
calcularPagamento :: Mao -> (Mao, Dinheiro) -> IO Dinheiro
calcularPagamento d (m, aposta)
    | isBlackjack m = if isBlackjack d
                        then do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++ ") é um Blackjack mas o " ++ "\x1b[36m" ++ "dealer " ++ "\x1b[0m" ++ "também tem um Blackjack. Empate"
                            return aposta
                        else do
                            putStrLn $ "Mão: " ++ show m ++ " é um Blackjack! Pagamento: " ++  show (bjPay aposta)
                            return $ bjPay aposta
    | valorDaMao m > 21 = do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++")" ++ "\x1b[31m" ++ " Estourou!" ++ "\x1b[0m"
                            return 0
    | valorDaMao d > 21 = do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++")" ++ "\x1b[32m" ++ " Ganhou!" ++ "\x1b[0m" ++ " Pagamento: " ++ show aposta
                            return $ 2 * aposta
    | valorDaMao m > valorDaMao d = do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++")" ++ "\x1b[32m" ++ " Ganhou!" ++ "\x1b[0m" ++ " Pagamento: " ++ show aposta
                            return $ 2 * aposta
    | valorDaMao m == valorDaMao d = do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++ ") Empate"
                            return aposta
    | otherwise = do
                            putStrLn $ "Mão: " ++ show m ++ " (" ++ show (valorDaMao m) ++")" ++ "\x1b[31m" ++ " Perde" ++ "\x1b[0m"
                            return 0

-- Processa a vez do dealer de comprar cartas até alcançar 16
turnoDoDealer :: JogoT IO ()
turnoDoDealer = do
    gs <- get
    let d = dealer gs
    liftIO $ putStrLn $ "O " ++ "\x1b[36m" ++ "dealer " ++ "\x1b[0m" ++  "tem " ++ show (mao d) ++ " para um valor de " ++ show (valorDaMao (mao d))
    if valorDaMao (mao d) < 17
        then do
            liftIO $ putStrLn ("\x1b[36m" ++ "Dealer " ++ "\x1b[0m" ++  "compra")
            (novaCarta, novoEstado) <- runState comprarCarta <$> get
            let novoDealer = d { mao = mao d ++ [novaCarta] }
            put $ novoEstado { dealer = novoDealer }
            turnoDoDealer
        else if valorDaMao (mao d) <= 21
            then do
                liftIO $ putStrLn $ ("\x1b[36m" ++ "Dealer " ++ "\x1b[0m" ++ "encerra com " ++ show (mao d) ++ " para um valor de " ++ show (valorDaMao (mao d)))
            else do
                liftIO $ putStrLn ("\x1b[36m" ++ "Dealer " ++ "\x1b[0m" ++ "estourou")

-- Se o dealer tiver um blackjack, todas as maos dos jogadores sao jogadas
processarBlackjackDoDealer :: JogoT IO ()
processarBlackjackDoDealer = do
    gs <- get
    let js = jogadores gs
    liftIO $ putStrLn ("O " ++ "\x1b[36m" ++ "dealer " ++ "\x1b[0m" ++ " tem um blackjack.")
    novosJogadores <- forM js $ \jogador -> do
        let maosAtivasJogador = maosAtivas jogador
        return $ jogador { maosJogadas = maosAtivasJogador, maosAtivas = [] }
    put $ gs { jogadores = novosJogadores }

-- Move todas as cartas para o monte de descarte e embaralha se atingir a penetracao
limparMaos :: JogoT IO ()
limparMaos = do
    gs <- get
    let js = jogadores gs
        d = dealer gs
        descartes = descarte gs
        monteDeDescarte = descartes ++ mao d ++ concatMap (concatMap fst . maosJogadas) js
    if length (baralho gs) < penetracao gs
        then do
            let (novoBaralho, gen') = embaralhar (monteDeDescarte ++ baralho gs) (gen gs)
            put gs { dealer = d {mao = []},
               jogadores = map (\jogador -> jogador { maosJogadas = [], seguro = 0 }) js,
               baralho = novoBaralho, gen = gen', descarte = [] }
            novapenetracao <- cartaDeCorte
            j2 <- get
            put $ j2 { penetracao = novapenetracao }
        else put $ gs { dealer = d { mao = []},
               jogadores = map (\jogador -> jogador { maosJogadas = [], seguro = 0 }) js,
               descarte = monteDeDescarte }

-- Se o jogador estiver sem dinheiro, ele é removido do jogo
limparJogadores :: JogoT IO ()
limparJogadores  = do
    gs <- get
    let js = jogadores gs
    novosJogadores <- filterM (\jogador -> do
        let bancaAtual = banca jogador
        if bancaAtual <= 0
            then do
                liftIO $ putStrLn $ "Jogador " ++ "\x1b[34m" ++ nomeDoJogador jogador ++ "\x1b[0m" ++ " ficou sem dinheiro."
                return False
            else do
                return True) js
    put $ gs { jogadores = novosJogadores }

solicitarJogadores :: IO [String]
solicitarJogadores = do
    putStrLn "Digite o número de jogadores:"
    entradaN <- getLine
    case readMaybe entradaN of
        Just n | n > 0 -> do
            solicitarNomes n
        _ -> do
            putStrLn "Entrada inválida. Por favor, digite um número inteiro positivo."
            solicitarJogadores

solicitarNomes :: Int -> IO [String]
solicitarNomes n = do
        replicateM n $ do
            putStrLn "Digite o nome do jogador:"
            getLine

-- Inicializa um novo jogo com uma semente aleatória e 6 baralhos
criarJogo :: [String] -> IO Jogo
criarJogo nomes = do
    let jogadoresIniciais = map (\nome -> Jogador nome [] [] 1000 0) nomes
    genInicial <- initStdGen
    let (novoBaralho, gen') = embaralhar (gerarBaralhos 6) genInicial
    let corte = 4 * length novoBaralho `div` 9
    return $ Jogo { baralho = novoBaralho,
                   descarte = [],
                   dealer = Dealer { nomeDoDealer = "José da Silva",
                                     mao = [],
                                     maoOculta = [] }, 
                   jogadores = jogadoresIniciais,
                   penetracao = corte,
                   gen = gen' }
