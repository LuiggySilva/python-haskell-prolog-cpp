{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import Control.Applicative ()
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics ( Generic )
import Control.Monad ( MonadPlus(mzero) )
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ()
import Control.Monad (mfilter)
import Numeric ( showFFloat )
import System.Random ( randomIO, randomRIO )
import Data.List ( sortBy )

data Produto = Produto {
    nome :: String,
    espaco :: Float,
    valor :: Float
} deriving (Show, Generic)

data Individuo = Individuo {
    espacos :: [Float],
    valores :: [Float],
    cromossomo :: [Int],
    espaco_usado :: Float,
    limite_espacos :: Float,
    geracao :: Int,
    nota_avaliacao :: Float
} deriving (Show) 

instance FromJSON Produto where
 parseJSON (Object v) =
    Produto <$> v .: "nome"
            <*> v .: "espaco"
            <*> v .: "valor"
 parseJSON _ = mzero

instance ToJSON Produto where
 toJSON (Produto nome espaco valor) =
    object [ "nome"   .= nome
           , "espaco" .= espaco
           , "valor"  .= valor ]

-- Retorna um intervalo de um array
sliceArray :: [Int] -> Int -> Int -> Int -> [Int]
sliceArray [] _ _ _ = []
sliceArray (h:t) x y cont
 | cont >= y = []
 | cont < x = sliceArray t x y (cont+1) 
 | otherwise = [h] ++ (sliceArray t x y (cont+1))
--

-- Concatena dois arrays
concatArray :: [Int] -> [Int] -> [Int]
concatArray a b = a ++ b

-- Converte um float para string
showFloat :: Float -> String
showFloat x = showFFloat Nothing x ""

-- Le um arquivo
read_File :: String -> IO B.ByteString
read_File path = B.readFile path

-- Extrai o arquivo json
readJSON :: Either String [t] -> [t]
readJSON entrada =
 case entrada of
  Left err -> []
  Right ps -> ps

-- Retorna a lista de produtos
getProdutos :: [Produto]
getProdutos = readJSON $ unsafePerformIO ( (eitherDecode <$> (read_File "../produtos.json")) :: IO (Either String [Produto]) )

-- Retorna o tamanho da maior string de nome, espaco e valor dos produtos 
getTamanhoStringProdutoComMaiorNome :: [Produto] -> Int -> Int
getTamanhoStringProdutoComMaiorNome [] aux = aux
getTamanhoStringProdutoComMaiorNome (h:t) aux 
 | length (nome h) > aux = getTamanhoStringProdutoComMaiorNome t (length (nome h))
 | otherwise = getTamanhoStringProdutoComMaiorNome t aux

getTamanhoStringProdutoComMaiorEspaco :: [Produto] -> Int -> Int
getTamanhoStringProdutoComMaiorEspaco [] aux = aux
getTamanhoStringProdutoComMaiorEspaco (h:t) aux 
 | length (showFloat (espaco h)) > aux = getTamanhoStringProdutoComMaiorEspaco t (length (showFloat (espaco h)))
 | otherwise = getTamanhoStringProdutoComMaiorEspaco t aux

getTamanhoStringProdutoComMaiorValor :: [Produto] -> Int -> Int
getTamanhoStringProdutoComMaiorValor [] aux = aux
getTamanhoStringProdutoComMaiorValor (h:t) aux 
 | length (showFloat (valor h)) > aux = getTamanhoStringProdutoComMaiorValor t (length (showFloat (valor h)))
 | otherwise = getTamanhoStringProdutoComMaiorValor t aux

getMaioresTamanhosStringProdutosInfo :: [Produto] -> [Int]
getMaioresTamanhosStringProdutosInfo p = [(getTamanhoStringProdutoComMaiorNome p 0)+1, (getTamanhoStringProdutoComMaiorEspaco p 0)+1, (getTamanhoStringProdutoComMaiorValor p 0)+1]
--

-- Imprime x espacos na mesma linha
imprimeEspacos :: Int -> IO()
imprimeEspacos 0 = putStr ""
imprimeEspacos x = do
  putStr " "
  imprimeEspacos (x-1)

-- Imprime os produtos
imprimeProdutosAux :: [Produto] -> [Int] -> IO()
imprimeProdutosAux [] _ = putStr ""
imprimeProdutosAux (h:t) [mn, me, mv] = do 
  putStr (nome h)
  imprimeEspacos (mn - (length (nome h)))
  putStr " - "

  putStr $ showFloat (valor h)
  imprimeEspacos (mv - (length $ showFloat (valor h)))
  putStr " - "

  putStr $ showFloat (espaco h)
  putStrLn ""

  imprimeProdutosAux t [mn, me, mv]

imprimeProdutos :: [Produto] -> IO()
imprimeProdutos produtos = do
  let t = getMaioresTamanhosStringProdutosInfo produtos 
  putStr "Produto"
  imprimeEspacos (t!!0 - 4)
  putStr "Valor"
  imprimeEspacos (t!!2 - 2)
  putStrLn "Espaco"
  imprimeProdutosAux produtos t
--

-- Gera um individuo
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (0,1)
  rs <- randomList (n-1) 
  return (r:rs)

initIndividuo :: [Float] -> [Float] -> Float -> Individuo
initIndividuo espacos valores limite = (Individuo espacos valores (unsafePerformIO (randomList (length espacos))) 0 limite 0 0)
--

-- Avalia um individuo
somaEspacoUsado :: [Float] -> [Int] -> Float
somaEspacoUsado [] [] = 0
somaEspacoUsado (e:t1) (c:t2)
 | c == 1 = e + (somaEspacoUsado t1 t2)
 | otherwise = somaEspacoUsado t1 t2

somaNotaAvaliacao :: [Float] -> [Int] -> Float
somaNotaAvaliacao [] [] = 0
somaNotaAvaliacao (e:t1) (c:t2)
 | c == 1 = e + (somaNotaAvaliacao t1 t2)
 | otherwise = somaNotaAvaliacao t1 t2

verificaSomaEspaco :: Float -> Float -> Float -> Float
verificaSomaEspaco soma_espaco limite nota
 | soma_espaco >= limite = 1.0
 | otherwise = nota

avaliacaoCromossomoIndividuo :: Individuo -> [Int] -> (Float, Float)
avaliacaoCromossomoIndividuo i cromossomo = do
 let soma_na = somaNotaAvaliacao (valores i) cromossomo
 let soma_espaco_usado = somaEspacoUsado (espacos i) cromossomo
 let soma_nota_avaliacao = verificaSomaEspaco soma_espaco_usado (limite_espacos i) soma_na
 (soma_nota_avaliacao, soma_espaco_usado)

avaliacaoIndividuo :: Individuo -> Individuo
avaliacaoIndividuo i = do
  let tuple = avaliacaoCromossomoIndividuo i (cromossomo i)
  (Individuo (espacos i) (valores i) (cromossomo i) (snd tuple) (limite_espacos i) (geracao i) (fst tuple))
--

-- Faz o crossover de invidivuos
corte :: Int -> Int
corte size 
 | x == 0 = corte size
 | otherwise = x 
 where x = unsafePerformIO(randomRIO (0, size))

crossoverIndividuo :: Individuo -> Individuo -> (Individuo, Individuo)
crossoverIndividuo a b = do
  let c = corte (length (cromossomo a) - 1)

  let filho1_pt1 = sliceArray (cromossomo b) 0 c 0 
  let filho1_pt2 = sliceArray (cromossomo a) c (length (cromossomo a)) 0
  let f1 = concatArray filho1_pt1 filho1_pt2

  let filho2_pt1 = sliceArray (cromossomo a) 0 c 0
  let filho2_pt2 = sliceArray (cromossomo b) c (length (cromossomo b)) 0
  let f2 = concatArray filho2_pt1 filho2_pt2

  let tuple1 = avaliacaoCromossomoIndividuo a f1
  let filho1 = Individuo (espacos a) (valores a) f1 (snd tuple1) (limite_espacos a) ((geracao a) + 1) (fst tuple1)

  let tuple2 = avaliacaoCromossomoIndividuo a f2
  let filho2 = Individuo (espacos a) (valores a) f1 (snd tuple2) (limite_espacos a) ((geracao a) + 1) (fst tuple2)
  
  (filho1, filho2)
--

-- Faz mutaçao em individuos
getCromossomoMutacao :: [Int] -> Float -> [Int]
getCromossomoMutacao [] _ = []
getCromossomoMutacao (h:t) taxa
 | (rand < taxa) && (h == 1) = [0] ++ (getCromossomoMutacao t taxa)
 | (rand < taxa) && (h == 0) = [1] ++ (getCromossomoMutacao t taxa)
 | otherwise = [h] ++ (getCromossomoMutacao t taxa)
 where rand = unsafePerformIO(randomIO)

mutacaoIndividuo :: Individuo -> Float -> Individuo
mutacaoIndividuo i taxa = do
  let novo_cromossomo = getCromossomoMutacao (cromossomo i) taxa
  (Individuo (espacos i) (valores i) novo_cromossomo (espaco_usado i) (limite_espacos i) (geracao i) (nota_avaliacao i))
--

-- Cria a populacao
initPopulacao :: Float -> [Float] -> [Float] -> Float -> [Individuo]
initPopulacao 0 _ _ _ = []
initPopulacao tamanho espacos valores limite_espacos = [initIndividuo espacos valores limite_espacos] ++ initPopulacao (tamanho-1) espacos valores limite_espacos

-- Retorna o melhor individuo
melhorIndividuo :: Individuo -> Individuo -> Individuo
melhorIndividuo a b 
 | (nota_avaliacao a) >= (nota_avaliacao b) = a 
 | otherwise = b

-- Ordena populacao
ordenaPopulacaoAux :: Individuo -> Individuo -> Ordering
ordenaPopulacaoAux i1 i2
 | (nota_avaliacao i1) > (nota_avaliacao i2) = GT
 | otherwise  = LT

ordenaPopulacao :: [Individuo] -> [Individuo]
ordenaPopulacao populacao = reverse (sortBy ordenaPopulacaoAux populacao)
--

-- Avalia populacao
avaliaPopulacao :: [Individuo] -> [Individuo]
avaliaPopulacao [] = []
avaliaPopulacao (h:t) = [avaliacaoIndividuo h] ++ (avaliaPopulacao t)

-- Soma avaliacoes dos individuos da populacao
somaAvaliacoes :: [Individuo] -> Float
somaAvaliacoes [] = 0
somaAvaliacoes (h:t) = (nota_avaliacao h) + (somaAvaliacoes t) 

-- Seleciona pai para o crossover 
selecionaPaiAux :: [Individuo] -> Int -> Float -> Float -> Int -> Int
selecionaPaiAux populacao pai_indice valor_sorteado soma i 
 | i < (length populacao) && soma < valor_sorteado = selecionaPaiAux populacao (pai_indice+1) valor_sorteado (soma + (nota_avaliacao (populacao!!i))) (i+1)
 | otherwise = pai_indice

selecionaPai :: [Individuo] -> Float -> Int
selecionaPai populacao soma_avaliacao = do
  let valor_sorteado = unsafePerformIO(randomIO) * soma_avaliacao
  (selecionaPaiAux populacao (-1) valor_sorteado 0 0)
--

-- Gera a visualizacao de um individuo
visualizaIndividuo :: Individuo -> IO()
visualizaIndividuo i = do
  putStr "Geracao: "
  print (geracao i)
  putStr "Nota avaliacao R$: "
  print (nota_avaliacao i)
  putStr "Espaco usado: "
  print (espaco_usado i)
  putStr "Cromossomo: " 
  print (cromossomo i)
  putStrLn ""

-- Gera a visualizacao de uma geracao
visualizaGeracao :: [Individuo] -> IO()
visualizaGeracao [] = putStr ""
visualizaGeracao populacao = visualizaIndividuo $ head populacao

-- Loop de geracoes
criaNovaPopulacao :: [Individuo] -> Int -> Float -> Float -> [Individuo]
criaNovaPopulacao populacao tamanho soma taxa
 | tamanho == 0 = []
 | otherwise = [f1, f2] ++ criaNovaPopulacao populacao (tamanho-1) soma taxa
 where pai1 = selecionaPai populacao soma
       pai2 = selecionaPai populacao soma
       filhos = crossoverIndividuo (populacao!!pai1) (populacao!!pai2) 
       f1 = mutacaoIndividuo (fst filhos) taxa
       f2 = mutacaoIndividuo (snd filhos) taxa

loopAux :: [Individuo] -> Float -> Float -> Float -> Individuo -> ([Individuo], Individuo)
loopAux populacao soma taxa tamanho melhor = do
  let t = floor $ tamanho / 2.0
  let nova_populacao = criaNovaPopulacao populacao t soma taxa
  let nova_populacao_avaliada = avaliaPopulacao nova_populacao
  let nova_populacao_ordenada = ordenaPopulacao nova_populacao_avaliada
  let melhor_individuo = head nova_populacao_ordenada
  (nova_populacao_avaliada, melhor_individuo)

looping :: Float -> Float -> Float -> Int -> Float -> Individuo -> [Individuo] -> [Individuo] 
looping _ _ _ 0 _ melhor_individuo _ = [melhor_individuo]
looping soma limite tamanho num_geracoes taxa melhor_individuo populacao = do
  let tuple = loopAux populacao soma taxa tamanho melhor_individuo
  (fst tuple) ++ (looping (somaAvaliacoes (fst tuple)) limite tamanho (num_geracoes-1) taxa (melhorIndividuo (snd tuple) melhor_individuo) (fst tuple))
--

-- Aux
print_evolucao :: [Individuo] -> IO()
print_evolucao [i] = do 
  putStrLn "Melhor solucao: "
  visualizaIndividuo i
print_evolucao (h:t) = do
  visualizaIndividuo h
  print_evolucao t

getEspacos :: [Produto] -> [Float]
getEspacos [] = []
getEspacos (h:t) = [(espaco h)] ++ (getEspacos t)

getValores :: [Produto] -> [Float]
getValores [] = []
getValores (h:t) = [(valor h)] ++ (getValores t)

getNomes :: [Produto] -> [String]
getNomes [] = []
getNomes (h:t) = [(nome h)] ++ (getNomes t)
--

getProdutosIndividuo :: [Produto] -> [Int] -> Int -> [Produto]
getProdutosIndividuo _ [] _ = []
getProdutosIndividuo produtos (1:t) index = [produtos!!index] ++ (getProdutosIndividuo produtos t (index+1))
getProdutosIndividuo produtos (_:t) index = getProdutosIndividuo produtos t (index+1)

-- Para instalar pacotes: cabal v1-install {pacote}
-- Para compilar: runhaskell genetic_algorithm.hs
main :: IO()
main = do 
  let tamanhoPopulacao = 20
  let limite = 3.0000
  let taxaMutacao = 0.1
  let geracoes = 100

  putStrLn ""
  putStr "Tamanho da populacao: "
  print tamanhoPopulacao
  putStr "Limite: "
  print limite
  putStr "Taxa mutacao: "
  putStrLn $ showFloat taxaMutacao
  putStr "Geracao: "
  print geracoes
  putStrLn ""

  let p = getProdutos
  imprimeProdutos p
  putStrLn ""

  let espacos = getEspacos p 
  let valores = getValores p 
  let nomes = getNomes p 

  let populacao = initPopulacao tamanhoPopulacao espacos valores limite
  let populacao_avaliada = avaliaPopulacao populacao
  let populacao_ordenada = ordenaPopulacao populacao_avaliada
  let melhor_individuo = head populacao_ordenada
  visualizaGeracao populacao_ordenada
  
  let evolucao = looping (somaAvaliacoes populacao_ordenada) limite tamanhoPopulacao geracoes taxaMutacao melhor_individuo populacao_ordenada
  print_evolucao evolucao
  let pi = getProdutosIndividuo p (cromossomo (evolucao!!((length evolucao)-1))) 0
  imprimeProdutos pi