import System.IO.Unsafe ( unsafePerformIO )
import System.IO ()
import Numeric ( showFFloat ) 
import Text.CSV
import Text.Parsec.Error
-- import Debug.Trace
-- Para debugar: trace <o que quer mostrar> <retorno da funcao/regra onde o trace esta sendo chamado>

data Flower = Flower {
 sepal_length :: Float,
 sepal_width :: Float,
 petal_length :: Float,
 petal_width :: Float,
 label :: String
} deriving (Show) 

-- Extrai o arquivo csv
readCSV :: Either ParseError [t] -> [t]
readCSV entrada =
 case entrada of
  Left err -> []
  Right ps -> ps

-- String to Float
toFloat :: [Char] -> Float
toFloat str = read str :: Float 

-- Converteo csv para o tipo Flower
csvToFlower :: [Record] -> [Flower]
csvToFlower [] = []
csvToFlower (h:t) = [Flower (toFloat $ h!!0) (toFloat $ h!!1) (toFloat $ h!!2) (toFloat $ h!!3) (h!!4)] ++ (csvToFlower t)

-- Retorna os inputs do dataset Iris
getX :: [Flower] -> [[Float]]
getX [] = []
getX (h:t) = [[sepal_length h, sepal_width h, petal_length h, petal_width h]] ++ (getX t)

-- Retorna os outputs do dataset Iris 
getY :: [Flower] -> [[Float]]
getY [] = []
getY (h:t)
 | (label h) == "Iris-setosa" = [[1]] ++ (getY t)
 | otherwise = [[0]] ++ (getY t)

 -- Retorna um intervalo de um array
sliceArray :: [Flower] -> Int -> Int -> Int -> [Flower]
sliceArray [] _ _ _ = []
sliceArray (h:t) x y cont
 | cont >= y = []
 | cont < x = sliceArray t x y (cont+1) 
 | otherwise = [h] ++ (sliceArray t x y (cont+1))

-- Converte um float para string
showFloat :: Float -> String
showFloat x = showFFloat Nothing x ""

-- Imprime x espacos na mesma linha
imprimeEspacos :: Int -> IO()
imprimeEspacos 0 = putStr ""
imprimeEspacos x = do
  putStr " "
  imprimeEspacos (x-1)

-- Retorna o tamanho do numero com mais digitos
getBiggestSizeM_Aux :: [Float] -> Int -> Int
getBiggestSizeM_Aux [] maior = maior
getBiggestSizeM_Aux (h:t) maior
 | tam > maior = getBiggestSizeM_Aux t tam
 | otherwise = getBiggestSizeM_Aux t maior
 where tam = length (showFloat h)  

getBiggestSizeM :: [[Float]] -> Int -> Int
getBiggestSizeM [] maior = maior
getBiggestSizeM (h:t) maior
 | tam > maior = getBiggestSizeM t tam
 | otherwise = getBiggestSizeM t maior
 where tam = (getBiggestSizeM_Aux h 0)

 --Imprime uma matriz
printMatrizAux3 :: Int -> Int -> IO()
printMatrizAux3 mSize index
 | index == mSize-1 = do
   putStr "]"
 | otherwise = do
   putStrLn "]"

printMatrizAux2 :: [Float] -> Int -> Int -> IO()
printMatrizAux2 [] _ _ = putStr ""
printMatrizAux2 [e] bigSize _ = do
 let str = showFloat e
 let strSize = length str
 let qtdE = bigSize - strSize
 imprimeEspacos qtdE
 putStr str
printMatrizAux2 (h:t) bigSize index = do
 let str = showFloat h
 let strSize = length str
 let qtdE = bigSize - strSize
 imprimeEspacos qtdE
 putStr (str ++ " ")
 printMatrizAux2 t bigSize (index+1)

printMatrizAux1 :: [[Float]] -> Int -> Int -> Int -> IO()
printMatrizAux1 [] _ _ _ = putStr ""
printMatrizAux1 (h:t) mSize bigSize index
 | index == 0 = do 
   putStr " ["
   printMatrizAux2 h bigSize 0
   printMatrizAux3 mSize 0
   printMatrizAux1 t mSize bigSize 1
 | otherwise = do
   putStr "  ["
   printMatrizAux2 h bigSize 0
   printMatrizAux3 mSize index
   printMatrizAux1 t mSize bigSize (index+1)

printMatriz :: [[Float]] -> IO()
printMatriz m = do
 let bigSize = getBiggestSizeM m 0
 putStr "["
 printMatrizAux1 m (length m) bigSize 0
 putStrLn " ]"

-- Imprime um vetor
printVectorAux2 :: Bool -> IO() 
printVectorAux2 True = do 
 putStrLn "]"
printVectorAux2 False = do
 putStr "] "

printVectorAux1 :: [Float] -> Int -> Int -> IO()
printVectorAux1 [] _ _ = putStr ""
printVectorAux1 (h:t) vSize index
 | index == vSize-1 = do
   putStr $ showFloat h
   printVectorAux1 t vSize (index+1)
 | otherwise = do
   putStr $ (showFloat h) ++ " "
   printVectorAux1 t vSize (index+1)

printVector :: [Float] -> Bool -> IO()
printVector vector endl = do
 let vSize = length vector
 putStr "["
 printVectorAux1 vector vSize 0
 printVectorAux2 endl

-- Matriz -> Vetor
matrixToVector :: [[t]] -> [t]
matrixToVector [] = []
matrixToVector (h:t) = h ++ (matrixToVector t)

-- Retorna um vetor com n zeros
vectorZeros :: Int -> [Float]
vectorZeros 0 = []
vectorZeros n = [0] ++ (vectorZeros (n-1))

-- Soma todos valores de um vetor
sumVector :: [Float] -> Float
sumVector [] = 0
sumVector (h:t) = h + (sumVector t)

-- Multiplica dois vetores
dot :: [Float] -> [Float] -> Float
dot [] [] = 0
dot (h1:t1) (h2:t2) = (h1 * h2) + (dot t1 t2)

-- Soma dois vetores
sumVectors :: [Float] -> [Float] -> [Float]
sumVectors [] [] = []
sumVectors (h1:t1) (h2:t2) = [h1 + h2] ++ (sumVectors t1 t2)

-- Subtrai dois vetores
subVectors :: [Float] -> [Float] -> [Float]
subVectors [] [] = []
subVectors (h1:t1) (h2:t2) = [h1 - h2] ++ (subVectors t1 t2)

-- Multiplica dois vetores (elemento por elemento)
multVectors :: [Float] -> [Float] -> [Float]
multVectors [] [] = []
multVectors (h1:t1) (h2:t2) = [h1 * h2] ++ (multVectors t1 t2)

-- Multiplica um vetor por uma constante 
multVectorByConstant :: [Float] -> Float -> [Float]
multVectorByConstant [] _ = []
multVectorByConstant (h:t) c = [h * c] ++ (multVectorByConstant t c)

-- Funcao de ativacao
stepActivationFunction :: Float -> [Float]
stepActivationFunction sum
 | sum > 0 = [1.0]
 | otherwise = [0.0]

-- Funcao de predicao
predict :: [Float] -> [Float] -> Float -> [Float]
predict inputs weights bias = do
 let sum = (dot inputs weights) + bias
 (stepActivationFunction sum)

-- Funcao de treinamento
trainAux :: [[Float]] -> [[Float]] -> [Float] -> Float -> Float -> ([Float], Float)
trainAux [] [] w b _ = (w, b)
trainAux (x:tx) (y:ty) weights bias learning_rate = do
 let prediction = predict x weights bias
 let error = subVectors y prediction
 let w = sumVectors weights (multVectorByConstant (multVectorByConstant x (head error)) learning_rate)
 let b = bias + sumVector (multVectorByConstant error learning_rate)
 (trainAux tx ty w b learning_rate)

train :: [[Float]] -> [[Float]] -> [Float] -> Float -> Float -> Int -> ([Float], Float)
train x y weights bias learning_rate epochs
 | epochs == 0 = (w, b)
 | otherwise = train x y w b learning_rate (epochs-1)
 where t = trainAux x y weights bias learning_rate
       w = fst t
       b = snd t 

main :: IO()
main = do 
 -- NAND Gate
 let x = [[0,0], [0,1], [1,0], [1,1]]
 let y = [[1], [1], [1], [0]]
 
 let number_of_inputs = 2
 let weights = vectorZeros number_of_inputs
 let bias = 0
 let lr = 0.01
 let epochs = 25

 let t = train x y weights bias lr epochs
 
 putStrLn "NAND Gate"
 putStrLn "X: "
 printMatriz x
 putStrLn "y: "
 printMatriz y

 putStrLn ""
 putStr "weights: "
 printVector (fst t) True
 putStrLn $ "bias: " ++ showFloat (snd t)

 putStrLn ""
 putStrLn "predicts: "
 putStr "X[0, 0], Y[1] Predict => "
 printVector (predict (x!!0) (fst t) (snd t)) True
 putStr "X[0, 1], Y[1] Predict => "
 printVector (predict (x!!1) (fst t) (snd t)) True
 putStr "X[1, 0], Y[1] Predict => "
 printVector (predict (x!!2) (fst t) (snd t)) True
 putStr "X[1, 1], Y[0] Predict => "
 printVector (predict (x!!3) (fst t) (snd t)) True

 -- Iris dataset
 putStrLn ""
 file <- parseCSVFromFile "../iris.csv"
 let raw_dataset = readCSV file
 let dataset = csvToFlower raw_dataset

 putStrLn "Iris dataset"
 putStrLn "X: "
 let x1_raw = sliceArray dataset 1 101 0
 let x1 = getX x1_raw
 printMatriz x1
 putStrLn "y: " 
 let y1_raw = sliceArray dataset 1 101 0
 let y1 = getY y1_raw
 printVector (matrixToVector y1) True

 let number_of_inputs1 = 4
 let weights1 = vectorZeros number_of_inputs1
 let bias1 = 0
 let lr1 = 0.01
 let epochs1 = 100

 let t1 = train x1 y1 weights1 bias1 lr1 epochs1

 putStrLn ""
 putStr "weights: "
 printVector (fst t1) True
 putStrLn $ "bias: " ++ showFloat (snd t1)

 putStrLn ""
 putStrLn "predicts: "
 putStr "X"
 printVector (x1!!0) False
 putStr "Y"
 printVector (y1!!0) False
 putStr "Predict => "
 printVector (predict (x1!!0) (fst t1) (snd t1)) True

 putStr "X"
 printVector (x1!!1) False
 putStr "Y"
 printVector (y1!!1) False
 putStr "Predict => "
 printVector (predict (x1!!1) (fst t1) (snd t1)) True

 putStr "X"
 printVector (x1!!50) False
 putStr "Y"
 printVector (y1!!50) False
 putStr "Predict => "
 printVector (predict (x1!!50) (fst t1) (snd t1)) True

 putStr "X"
 printVector (x1!!51) False
 putStr "Y"
 printVector (y1!!51) False
 putStr "Predict => "
 printVector (predict (x1!!51) (fst t1) (snd t1)) True