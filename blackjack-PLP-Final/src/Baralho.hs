module Baralho where

import System.Random (StdGen, randomR)

data Valor = As | Dois | Tres | Quatro | Cinco | Seis |
            Sete | Oito | Nove | Dez | Valete | Dama | Rei
            deriving (Show, Eq, Enum)

data Naipe = Paus | Ouros | Copas | Espadas
            deriving (Show, Eq, Enum)

data Carta = Carta Valor Naipe
            deriving (Show, Eq)

instance Ord Carta where
    compare (Carta v1 _) (Carta v2 _) = compare (valorDaCarta (Carta v1 Paus)) (valorDaCarta (Carta v2 Paus))

type Baralho = [Carta]

-- Gera um baralho completo com 52 cartas
baralhoCompleto :: [Carta]
baralhoCompleto = [Carta valor naipe | naipe <- [Paus .. Espadas], valor <- [As .. Rei]]

-- Gerar quantos baralhos seja necessário
gerarBaralhos :: Int -> [Carta]
gerarBaralhos n = concat $ replicate n baralhoCompleto

-- Embaralha o baralho para o jogo
embaralhar :: Baralho -> StdGen -> (Baralho, StdGen)
embaralhar [] g = ([], g)
embaralhar baralho g =
    let (indice, novaGen) = randomR (0, length baralho - 1) g
        carta = baralho !! indice
        resto = take indice baralho ++ drop (indice + 1) baralho
    in (carta : fst (embaralhar resto novaGen), novaGen)

-- Compra uma carta do baralho
comprarCarta :: Baralho -> (Carta, Baralho)
comprarCarta baralho = (head baralho, tail baralho)

-- Faça Ás valer 1 e use lógica nas regras para determinar se deve ser 1 ou 11
valorDaCarta :: Carta -> Int
valorDaCarta (Carta valor _) =
    case valor of
        As     -> 1
        Dois   -> 2
        Tres   -> 3
        Quatro -> 4
        Cinco  -> 5
        Seis   -> 6
        Sete   -> 7
        Oito   -> 8
        Nove   -> 9
        _      -> 10

-- Vizualização para carta
cartaString :: Carta -> String
cartaString (Carta valor naipe) =
    "+---------+\n" ++
    "| " ++ num ++ replicate (7 - length num) ' ' ++ " |\n" ++
    "|         |\n" ++
    "|         |\n" ++
    "|    " ++ naipe' ++ "    |\n" ++
    "|         |\n" ++
    "|         |\n" ++
    "| " ++ replicate (7 - length num) ' ' ++ num ++ " |\n" ++
    "+---------+\n"
  where
    num = if valorCartaToString (Carta valor naipe) == "10" then "10" else take 1 (valorCartaToString (Carta valor naipe))
    naipe' = case naipe of
        Espadas -> "♠"
        Ouros   -> "♦"
        Copas   -> "♥"
        Paus    -> "♣"

-- Vizualização de uma lista de cartas para String
cartaASCII :: [Carta] -> String
cartaASCII = unlines . map cartaString

-- Transforma valor de uma carta em String
valorCartaToString :: Carta -> String
valorCartaToString (Carta x _) =
    case x of
        As     -> "A"
        Dois   -> "2"
        Tres   -> "3"
        Quatro -> "4"
        Cinco  -> "5"
        Seis   -> "6"
        Sete   -> "7"
        Oito   -> "8"
        Nove   -> "9"
        Dez    -> "10"
        Rei    -> "K"
        Dama   -> "Q"
        Valete -> "J"
