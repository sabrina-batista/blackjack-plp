module Tipos (
    Jogo (..),
    JogoT,
    Dinheiro,
    Mao,
    Jogador (..),
    Dealer (..)
) where

import Baralho ( Baralho, Carta )
import System.Random ( StdGen )
import Control.Monad.Trans.State ( StateT )

data Jogo = Jogo
    { baralho    :: Baralho
    , descarte :: Baralho
    , dealer     :: Dealer
    , jogadores  :: [Jogador]
    , penetracao :: Int
    , gen    :: StdGen}
    deriving (Show)

type Dinheiro = Int

type Mao = [Carta]

data Jogador = Jogador { nomeDoJogador  :: String
                      , maosAtivas :: [(Mao, Dinheiro)]
                      , maosJogadas :: [(Mao, Dinheiro)]
                      , banca     :: Dinheiro 
                      , seguro    :: Dinheiro }
    deriving (Eq)

data Dealer = Dealer { nomeDoDealer :: String
                     , mao       :: Mao
                     , maoOculta :: Mao }
    deriving (Eq)

instance Show Jogador where
    show (Jogador nome ma ph bn _) = "--------------------\nJogador: " ++ nome ++
                                  "\nMãos: " ++ unwords [show m ++ "\t" | (m,_) <- ma ++ ph] ++
                                  "\nApostas: " ++ unwords [show d ++ "\t" | (_,d) <- ma ++ ph] ++
                                  "\nBanca: " ++ show bn ++
                                  "\n--------------------"
                                  
instance Show Dealer where
    show (Dealer nome m mo)    = if null mo
                                    then "------------------------------\nDealer: " ++ nome ++
                                         "\nMão: " ++ show m ++
                                         "\n------------------------------"
                                    else "------------------------------\nDealer: " ++ nome ++
                                         "\nMão: " ++ maoAberta ++
                                         "\n------------------------------"
        where
            maoAberta = show m ++ ", ###"

type JogoT m = StateT Jogo m