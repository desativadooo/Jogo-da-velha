module Auxiliar where

import Control.Exception
import System.IO.Error
import System.IO
import System.Process
import Data.List
import System.Random
import Data.Function

-- Tipos de dados utilizados
type Jogadores = [Jogador]
type Nome = String
type Pontuacao = Int
type Jogada = Int
type Vez = Int
type Tabela = String
data Jogador = Jogador Nome Pontuacao
  deriving (Show , Read)

-- FUNÇÕES AUXILIARES

-- funções auxiliares para configurar o jogador

coletarNome :: String -> IO String
coletarNome nome = do
 putStr nome
 res <- getLine  -- vai coletar o nome do jogador
 return res

nomeExistente :: Jogadores -> Nome -> Bool
nomeExistente [] _= False
nomeExistente ((Jogador x y):xs) nome -- vai verificar se o nome do jogador existe
 | x == nome = True
 | otherwise = nomeExistente xs nome

 -- funções auxiliares para configurar o tabuleiro
converter :: Int -> Char
converter x = head (show x)

obterNovoTabuleiro :: Tabela -> Vez -> Char -> Tabela
obterNovoTabuleiro [] _ _ = ['7']
obterNovoTabuleiro (x:xs) vez y
 | (x == y) && (vez == 0) = ['X'] ++ xs
 | (x == y) && (vez == 1) = ['O'] ++ xs
 | otherwise = x:(obterNovoTabuleiro xs vez y)

venceuJogador1 :: Tabela -> Bool
venceuJogador1 tabela
 | (tabela !! 0) == 'X' && (tabela !! 1) == 'X' && (tabela !! 2) == 'X' = True
 | (tabela !! 3) == 'X' && (tabela !! 4) == 'X' && (tabela !! 5) == 'X' = True
 | (tabela !! 6) == 'X' && (tabela !! 7) == 'X' && (tabela !! 8) == 'X' = True
 | (tabela !! 0) == 'X' && (tabela !! 3) == 'X' && (tabela !! 6) == 'X' = True
 | (tabela !! 1) == 'X' && (tabela !! 4) == 'X' && (tabela !! 7) == 'X' = True
 | (tabela !! 2) == 'X' && (tabela !! 5) == 'X' && (tabela !! 8) == 'X' = True
 | (tabela !! 2) == 'X' && (tabela !! 4) == 'X' && (tabela !! 6) == 'X' = True
 | (tabela !! 0) == 'X' && (tabela !! 4) == 'X' && (tabela !! 8) == 'X' = True
 | otherwise = False

venceuJogador2 :: Tabela -> Bool
venceuJogador2 tabela
 | (tabela !! 0) == 'O' && (tabela !! 1) == 'O' && (tabela !! 2) == 'O' = True
 | (tabela !! 3) == 'O' && (tabela !! 4) == 'O' && (tabela !! 5) == 'O' = True
 | (tabela !! 6) == 'O' && (tabela !! 7) == 'O' && (tabela !! 8) == 'O' = True
 | (tabela !! 0) == 'O' && (tabela !! 3) == 'O' && (tabela !! 6) == 'O' = True
 | (tabela !! 1) == 'O' && (tabela !! 4) == 'O' && (tabela !! 7) == 'O' = True
 | (tabela !! 2) == 'O' && (tabela !! 5) == 'O' && (tabela !! 8) == 'O' = True
 | (tabela !! 2) == 'O' && (tabela !! 4) == 'O' && (tabela !! 6) == 'O' = True
 | (tabela !! 0) == 'O' && (tabela !! 4) == 'O' && (tabela !! 8) == 'O' = True
 | otherwise = False

atualizarPontuacao :: Jogadores -> String -> Jogadores
atualizarPontuacao ((Jogador nome pontuacao):xs) vencedor
 | (nome == vencedor) = [Jogador nome (pontuacao + 1)] ++ xs
 | otherwise = (Jogador nome pontuacao):(atualizarPontuacao xs vencedor)

-- funções auxiliares para configurar o ranking

obterNome :: Jogador -> Nome
obterNome (Jogador nome _) = nome

obterPontuacao :: Jogador -> Pontuacao
obterPontuacao (Jogador _ pontuacao) = pontuacao

ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterPontuacao) dados
