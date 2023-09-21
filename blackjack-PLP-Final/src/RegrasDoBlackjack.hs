-- Funções auxiliares para resolver mãos
module RegrasDoBlackjack (
    possuiAs,
    valorDaMao,
    dealerBlackjack,
    isBlackjack,
    bjPay
) where

import Baralho (Carta(Carta), Valor(As), valorDaCarta)
import Tipos (Dealer(maoOculta, mao), Mao, Dinheiro)

-- Soma o valor das cartas na mão do jogador
somaMao :: Mao -> Int
somaMao [] = 0
somaMao ms = sum $ map valorDaCarta ms

-- Verifica se o jogador possui um As
possuiAs :: Mao -> Bool
possuiAs = any (\(Carta f _) -> f == As)

-- Verifica se uma mão pode receber o valor 11 para um As
isMaoFlexivel :: Mao -> Bool
isMaoFlexivel ms =  (somaMao ms + 10 <= 21) && possuiAs ms

-- Calcula os pontos da mão
valorDaMao :: Mao -> Int
valorDaMao ms = if isMaoFlexivel ms
                    then somaMao ms + 10
                    else somaMao ms

-- Verifica se a mão é blackjack
isBlackjack :: Mao -> Bool
isBlackjack m = (valorDaMao m == 21) && (length m == 2)

-- Verifica se o dealer tem blackjack
dealerBlackjack :: Dealer -> Bool
dealerBlackjack d = length h == 2 && valorDaMao h == 21
    where 
        h = mao d ++ maoOculta d

-- Calcula os ganhos de um blackjack
bjPay :: Dinheiro -> Dinheiro
bjPay b = 3*b `div` 2