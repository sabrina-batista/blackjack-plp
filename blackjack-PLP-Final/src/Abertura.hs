module Abertura (
    distribuirMaosIniciais
) where

import Tipos
    ( Dealer(maoOculta, mao),
      Jogador(maosAtivas),
      Jogo(dealer, jogadores) )
import Acoes  (comprarCarta)
import Control.Monad.Trans.State ( get, put, State )
import Control.Monad (forM)

-- Distribuir uma carta para as apostas de cada jogador, depois para o dealer e, em seguida, outra carta para cada mão
-- em seguida, a carta final do dealer fica oculta
distribuirMaosIniciais :: State Jogo ()
distribuirMaosIniciais = do
    gs <- get
    let js = jogadores gs
    let d = dealer gs
    novosJogadores <- distribuirCartaParaTodosOsJogadores js
    novaCarta <- comprarCarta
    novosJogadores2 <- distribuirCartaParaTodosOsJogadores novosJogadores
    novaCarta2 <- comprarCarta
    let novoDealer2 = d { mao = [novaCarta], maoOculta = [novaCarta2] }
    gs2 <- get
    put $ gs2 { jogadores = novosJogadores2, dealer = novoDealer2 }

-- Distribuir uma carta para a mão de cada jogador usando comprarCarta
-- A mão ativa é uma mão com uma aposta ativa
distribuirCartaParaTodosOsJogadores :: [Jogador] -> State Jogo [Jogador]
distribuirCartaParaTodosOsJogadores gs = do
    forM gs $ \j -> do
        novasMaosApostadas <- forM (maosAtivas j) $ \(m, a) -> do
            novaCarta <- comprarCarta
            return (m ++ [novaCarta], a)
        return j { maosAtivas = novasMaosApostadas }