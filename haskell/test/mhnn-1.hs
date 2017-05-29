module Main
where

-- ask why these imports needed
import Prelude                        as P
import Data.List                      (mapAccumL)

import Data.Array.Accelerate          as A
import GHC.Float
import Data.Array
import Control.Monad
import System.Environment

import Data.Binary.Get
import Data.Word

-- trying to translate coursera machine learning neural network 
-- into accelerate or haskell version...

-- Robs
sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))

-- http://stackoverflow.com/questions/9748474/reading-from-input-files-in-haskell
readWords :: [String] -> [Float]
readWords [] = []
readWords (x:xs) = [read x :: Float] P.++ readWords xs

--parseFile :: String -> Int -> Int -> IO (Array (Int, Int) Int)
--parseFile filename rows cols = do
--    matrix <- liftM readWords $ readFile filename
--    return $ listArray ((1,1), (rows, cols)) matrix

loadDataMatrix :: IO ([Float])
loadDataMatrix = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    let dbarr = readWords strarr
    return dbarr

loadDataMatrixA :: IO (A.Array DIM2 Float)
loadDataMatrixA = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    let dbarr = readWords strarr
    let arr = A.fromList (Z:.400:.100) dbarr :: A.Array DIM2 Float
    return arr

loadThetaA :: IO (A.Array DIM2 Float)
loadThetaA = do
    content <- readFile "trainlabel100.txt"
    let strarr = words content
    let dbarr = readWords strarr
    let arr = A.fromList (Z:.100:.1) dbarr :: A.Array DIM2 Float
    return arr

-- matlab function: lrCostFunction(theta, X, y, lambda) = [J, grad]
lrCostFunction :: A.Array DIM2 Float -> A.Array DIM2 Float -> A.Array DIM1 Float -> Exp Float -> (Exp Float, A.Array DIM2 Float)
rCostFunction theta X y lambda = (J, grad)
    where
--      J = (1/m) * sum(-y.*(log(sigmoid(X * theta))) - (1 - y).*log(1-sigmoid(X*theta))) + (lambda/(2*m))*(sum(temp.^2))
		J = (1/m) * sum (firstbit - secondbit) + regularization
        firstbit = A.zipWith (*) (use (-y)) (use (map (log . sigmoid) (X * theta)))
        secondbit = A.zipWith (*) (1 - y) (use (map log (1 - sigmoid (X * theta))))
        regularization = lambda / (2*m) * (foldr 0 (x^2 | x <- temp))
        temp = 0 -- TODO
        lambda = 0 -- TODO
        m = A.length (use y) -- y is a vector because it is the number of training examples