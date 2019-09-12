import Control.Exception
import System.IO.Error
import System.IO
import System.Process
import Data.List
import System.Random
import Data.Function
import Auxiliar

-- FUNÇÕES PRINCIPAIS

-- função para gerenciar o menu do jogo

menu :: Jogadores -> IO Jogadores
menu dados = do
 system "cls"
 putStrLn "..................... Jogo da velha ....................."
 putStrLn "\n[1] cadastrar jogador"
 putStrLn "[2] dois Jogadores"
 putStrLn "[3] um jogador"
 putStrLn "[4] ranking"
 putStrLn "[0] sair"
 putStrLn "Opção: "
 opcao <- getChar
 getChar
 executarOpcao dados opcao

-- função para executar as opções do menu

executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = (cadastrarJogador dados)
executarOpcao dados '2' = (prepararJogo dados)
executarOpcao dados '4' = do
  putStrLn "\nRanking dos jogadores: "
  if (null dados) then do
       putStrLn ("Não há jogadores cadastrados")
  else 
  	   (mostrarRank (reverse (ordenar dados)))
  putStrLn "\nPressione <enter> para voltar"
  getChar
  menu dados
executarOpcao dados '3' = (prepararJogo2 dados)
executarOpcao dados '0' = do
 putStrLn "\n acabou!"
 return dados
executarOpcao dados _ = do
 putStrLn ("\nOpção inválida! tente novamente.")
 putStrLn "\nPressione enter para voltar ao menu."
 getChar
 menu dados

-- função para cadastrar jogador

cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do 
 nome <- (coletarNome "Digite um nome para o jogador: ")
 if (nomeExistente dados nome) then do
    putStrLn "\nEsse nome já esta cadastrado."
    getChar
    menu dados
 else do
    arquivo <- openFile "dados.txt" WriteMode
    hPutStrLn arquivo (show ((Jogador nome 0): dados))
    hClose arquivo
    putStrLn ("\njogador " ++ nome ++ " cadastrado com sucesso.")
    putStr "\nPressione enter para voltar."
    getChar
    menu ((Jogador nome 0):dados)

-- função para preparar o jogo no modo (dois jogadores)

prepararJogo :: Jogadores -> IO Jogadores
prepararJogo dados = do 
 jogador1 <- coletarNome "\nDigite o nome do primeiro jogador: " 
 if not (nomeExistente dados jogador1) then do
    putStrLn "\nEsse nome nao esta cadastrado."
    getChar
    menu dados
 else do
    jogador2 <- coletarNome "\nDigite o nome do segundo jogador: "
    if not (nomeExistente dados jogador2) then do
       putStrLn "\nEsse nome nao esta cadastrado."
       menu dados
    else do 
       novoJogo dados jogador1 jogador2 

-- função para iniciar um novo jogo no modo (dois jogadores)

novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
 putStrLn ("\nCarregando..... " ++ jogador1 ++ " vs " ++ jogador2 ++ " .....")
 putStrLn ("\nOs quadrados que possuem números, não estão marcados ")
 putStrLn ("\n"++ jogador1 ++ " será X e " ++ jogador2 ++ " será 0")
 rodarJogo dados ['1','2','3','4','5','6','7','8','9'] jogador1 jogador2 0

-- função para rodar o jogo no modo (dois jogadores)

rodarJogo :: Jogadores -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
rodarJogo dados tabela jogador1 jogador2 vez = do
 putStrLn ("\n" ++ "                              " ++
 	           (show (tabela !! 0))++ "  |  " ++(show (tabela !! 1))++ "  |  " ++ (show(tabela !! 2)) ++
           "\n" ++ "                             " ++
               (show (tabela !! 3))++ "  |  " ++(show (tabela !! 4))++ "  |  " ++ (show(tabela !! 5)) ++
           "\n" ++ "                             " ++
               (show (tabela !! 6))++ "  |  " ++(show (tabela !! 7))++ "  |  " ++ (show(tabela !! 8)) ++
           "\n")
 if (venceuJogador1 tabela) then do                         -- caso o jogador1 vença vai executar isso
      putStrLn ("\nMeus parabens " ++ jogador1 ++"! voce venceu!")
      arquivoEscrito <- openFile "dados.txt" WriteMode
      hPutStrLn arquivoEscrito (show(atualizarPontuacao dados jogador1))
      hClose arquivoEscrito
-- vai abrir o arquivo pra leitura
      arquivoLeitura <- openFile "dados.txt" ReadMode
      dadosAtualizados <- hGetLine arquivoLeitura
      hClose arquivoLeitura
      putStr "\nPressione enter para voltar ao menu."
      getChar
      menu (read dadosAtualizados)
 else do -- caso o jogador2 vença vai executar isso
      if (venceuJogador2 tabela) then do
           putStrLn ("\nMeus parabens " ++ jogador2 ++"! voce venceu!")
           arquivoEscrito <- openFile "dados.txt" WriteMode
           hPutStrLn arquivoEscrito (show(atualizarPontuacao dados jogador2))
           hClose arquivoEscrito
-- vai abrir o arquivo pra leitura
           arquivoLeitura <- openFile "dados.txt" ReadMode
           dadosAtualizados <- hGetLine arquivoLeitura
           hClose arquivoLeitura
           putStr "\nPressione enter para voltar ao menu."
           getChar
           menu (read dadosAtualizados)
      else do
           if (length (intersect "123456789" tabela) == 0) then do
   	            putStrLn ("\nEmpate!")
   	            getChar
   	            menu dados
   	       else do
   	            if (vez == 0) then do
   	                 putStr (jogador1 ++ ", é a sua vez, escolha um número ")
   	                 opcao <- getChar
   	                 getChar
   	                 if not (elem opcao "123456789") then do
   	                      putStrLn "\nOpçao nao valida, escolha um dos números"
   	                      rodarJogo dados tabela jogador1 jogador2 0
   	                 else
   	                      if not (elem opcao tabela) then do
   	                           putStrLn "\nO lugar ja foi selecionado, escolha outro"
   	                           rodarJogo dados tabela jogador1 jogador2 0
   	                      else
   	       	                   rodarJogo dados (obterNovoTabuleiro tabela vez opcao) jogador1 jogador2 1
   	            else do   
                     putStr (jogador2 ++ " , é a sua vez, escolha um número ")
                     opcao <- getChar
                     getChar
                     if not (elem opcao "123456789") then do
                          putStrLn "\nOpçao nao valida, escolha um dos números"
                          rodarJogo dados tabela jogador1 jogador2 1
   	                 else
   	                      if not (elem opcao tabela) then do
   	                           putStrLn "\nO lugar ja foi selecionado, escolha outro"
   	                           rodarJogo dados tabela jogador1 jogador2 1
   	                      else
   	        	               rodarJogo dados (obterNovoTabuleiro tabela vez opcao) jogador1 jogador2 0


-- função para preparar o jogo no modo (um jogador)

prepararJogo2 :: Jogadores -> IO Jogadores
prepararJogo2 dados = do
 jogador1 <- coletarNome "\nDigite o nome do primeiro jogador: " --Vai checar se o nome ta cadastrado.
 if not (nomeExistente dados jogador1) then do
    putStrLn "\nEsse nome nao esta cadastrado."
    getChar
    menu dados
 else
    novoJogo2 dados jogador1

-- função para iniciar um novo jogo no modo (um jogador)

novoJogo2 :: Jogadores -> Nome -> IO Jogadores
novoJogo2 dados jogador1 = do
  putStrLn ("\nCarregando..... " ++ jogador1 ++ " vs " ++ "I.A. .....")
  putStrLn ("\nOs quadrados que possuem números, não estão marcados ")
  putStrLn ("\n"++ jogador1 ++ " será X e I.A. será 0")
  jogarComIa dados ['1','2','3','4','5','6','7','8','9'] jogador1 0

-- função para rodar o jogo no modo (um jogador)

jogarComIa :: Jogadores -> Tabela -> Nome -> Vez -> IO Jogadores
jogarComIa dados tabela jogador1 vez = do
 numero <- randomRIO (1::Int, 9) 
 putStrLn ("\n" ++ "                              " ++
             (show (tabela !! 0))++ "  |  " ++(show (tabela !! 1))++ "  |  " ++ (show(tabela !! 2)) ++
           "\n" ++ "                             " ++
               (show (tabela !! 3))++ "  |  " ++(show (tabela !! 4))++ "  |  " ++ (show(tabela !! 5)) ++
           "\n" ++ "                             " ++
               (show (tabela !! 6))++ "  |  " ++(show (tabela !! 7))++ "  |  " ++ (show(tabela !! 8)) ++
           "\n")
 if (venceuJogador1 tabela) then do                         -- caso o jogador1 vença vai executar isso
       putStrLn ("\nMeus parabens " ++ jogador1 ++"! voce venceu!")
       arquivoEscrito <- openFile "dados.txt" WriteMode
       hPutStrLn arquivoEscrito (show(atualizarPontuacao dados jogador1))
       hClose arquivoEscrito
-- vai abrir o arquivo pra leitura
       arquivoLeitura <- openFile "dados.txt" ReadMode
       dadosAtualizados <- hGetLine arquivoLeitura
       hClose arquivoLeitura
       putStr "\nPressione enter para voltar ao menu."
       getChar
       menu (read dadosAtualizados)
 else do -- caso o jogador2 vença vai executar isso
      if (venceuJogador2 tabela) then do
           putStrLn ("\nDessa vez a I.A. Ganhou")
           arquivoEscrito <- openFile "dados.txt" WriteMode
           hPutStrLn arquivoEscrito (show(atualizarPontuacao dados jogador1))
           hClose arquivoEscrito
-- vai abrir o arquivo pra leitura
           arquivoLeitura <- openFile "dados.txt" ReadMode
           dadosAtualizados <- hGetLine arquivoLeitura
           hClose arquivoLeitura
           putStr "\nPressione enter para voltar ao menu."
           getChar
           menu (read dadosAtualizados)
      else do
           if (length (intersect "123456789" tabela) == 0) then do
                putStrLn ("\nEmpate!")
                getChar
                menu dados
           else do
                if (vez == 0) then do
                     putStr (jogador1 ++ ", é a sua vez, escolha um número: ")
                     opcao <- getChar
                     getChar
                     if not (elem opcao "123456789") then do
                          putStrLn "\nOpçao nao valida, escolha um dos números"
                          jogarComIa dados tabela jogador1 0
                     else
                          if not (elem opcao tabela) then do
                               putStrLn "\nO lugar ja foi selecionado, escolha outro"
                               jogarComIa dados tabela jogador1 0
                          else
                               jogarComIa dados (obterNovoTabuleiro tabela vez opcao) jogador1 1
                else do
                     if not (elem (converter numero) tabela) then do
                          jogarComIa dados (obterNovoTabuleiro tabela vez (converter numero)) jogador1 1
                     else do
                     	putStrLn ("Vez do computador: " ++ show numero)
                        jogarComIa dados (obterNovoTabuleiro tabela vez (converter numero)) jogador1 0


-- função que vai exibir o ranking dos jogadores

mostrarRank :: Jogadores -> IO ()
mostrarRank [] = return ()
mostrarRank (x:xs) = do
	putStrLn ((obterNome x) ++ " possui "++ (show(obterPontuacao x))++ " pontos ")
	mostrarRank xs

-- Função para iniciar o programa

jogar :: IO ()
jogar = do
    {catch (lerArquivo) tratarErro;}
    where
    	-- vamos tentar ler o arquivo
    	lerArquivo = do
        {
             arquivo <- openFile "dados.txt" ReadMode;
             dados <- hGetLine arquivo;
             hClose arquivo;
             menu (read dados);
             return ()
        }
        tratarErro erro = if isDoesNotExistError erro then do
        {
        
           arquivo <- openFile "dados.txt" WriteMode;
           hPutStrLn arquivo "[]";
           hClose arquivo;
           menu [] ;
           return ()
        }
        else
            ioError erro
