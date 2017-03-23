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
--sigmoid :: Exp Float -> Exp Float
sigmoid :: Float -> Float
sigmoid z = 1.0 / (1.0 + exp(-z))

-- http://stackoverflow.com/questions/9748474/reading-from-input-files-in-haskell
readWords :: [String] -> [Double]
readWords [] = []
readWords (x:xs) = [read x :: Double] P.++ readWords xs

--parseFile :: String -> Int -> Int -> IO (Array (Int, Int) Int)
--parseFile filename rows cols = do
--    matrix <- liftM readWords $ readFile filename
--    return $ listArray ((1,1), (rows, cols)) matrix

loadData :: IO ([Double])
loadData = do
    content <- readFile "trainingsample.txt"
    let strarr = words content
    let dbarr = readWords strarr
    return dbarr

loadDataAcc :: IO (A.Array DIM2 Double)
loadDataAcc = do
    content <- readFile "trainingsample.txt"
    let strarr = words content
    let dbarr = readWords strarr
    let arr = A.fromList (Z:.5000:.400) dbarr :: A.Array DIM2 Double
    return arr
