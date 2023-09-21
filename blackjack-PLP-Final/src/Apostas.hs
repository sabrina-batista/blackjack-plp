module Apostas (
  getApostas
) where

import Tipos
import Data.List.Split
import Data.Char (isDigit)

-- Pergunta pelas apostas e coloca todas as apostas em activeHands para esse jogador
solicitarApostas :: Jogador -> IO Jogador
solicitarApostas j = do
  let maxApostas = 4 :: Int
  putStrLn $ "Digite até " ++ show maxApostas ++ " apostas para o jogador " ++ "\x1b[34m" ++ nomeDoJogador j ++ "\x1b[0m" ++ " separadas por vírgulas:"
  apostasStr <- getLine
  let apostas = analisarApostas apostasStr
  if saoApostasValidas apostas (banca j)
    then
      return $ j { maosAtivas = [([], a) | a <- apostas], banca = banca j - sum apostas }
    else do
      putStrLn "Apostas inválidas. Por favor, tente novamente."
      solicitarApostas j

-- Analisa uma string de apostas em uma lista de valores Dinheiro
analisarApostas :: String -> [Dinheiro]
analisarApostas str
  | all isDigitAndComma str = map read $ filter (not . null) $ splitOn "," str
  | otherwise = []
  where isDigitAndComma c = isDigit c || c == ','

-- Obtém uma string de aposta para cada jogador
getApostas :: [Jogador] -> IO [Jogador]
getApostas [] = return []
getApostas (j:js) = do
  j' <- solicitarApostas j
  js' <- getApostas js
  return (j':js')

-- Verifica se uma lista de apostas é válida para um jogador
saoApostasValidas :: [Dinheiro] -> Dinheiro -> Bool
saoApostasValidas [] _ = False
saoApostasValidas apostas br = all (> 0) apostas && length apostas <= 4 && sum apostas <= br