{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Main where

import MHNN
import Data.Load.MINST

import Prelude                                            as P
import Control.DeepSeq
import Data.Time.Clock
import System.CPUTime
import System.IO
import Text.Printf
-- import Criterion.Main

import Data.Array.Accelerate                              ( Array, Acc, Exp, Z(..), (:.)(..), DIM1, DIM2 )
import qualified Data.Array.Accelerate                    as A
import qualified Data.Array.Accelerate.Debug              as A
import qualified Data.Array.Accelerate.LLVM.Native        as CPU


main :: IO ()
main = do
  -- Network configuration
  -- ---------------------
  let hiddenLayers    = 300   -- pick a number, any number!
      outputLayers    = 10    -- number of output labels to train the network to recognise
      regularisation  = 1.0
      --
      trainImagesFile = "../data/train-images-idx3-ubyte"
      trainLabelsFile = "../data/train-labels-idx1-ubyte"
      testImagesFile  = "../data/t10k-images-idx3-ubyte"
      testLabelsFile  = "../data/t10k-labels-idx1-ubyte"

  -- No more configuration below this point
  -- --------------------------------------

  -- Load training and test data sets
  trainImages <- loading loadImages' trainImagesFile
  trainLabels <- loading loadLabels' trainLabelsFile
  testImages  <- loading loadImages' testImagesFile
  testLabels  <- loading loadLabels' testLabelsFile

  let Z :. trainSamples :. inputLayers  = A.arrayShape trainImages
      Z :. testSamples                  = A.arrayShape testLabels

  -- Generate some random weights between the input->hidden layer, and between
  -- the hidden->output layer
  theta1  <- timed "generating theta1" $ (CPU.run <$> gentheta2 inputLayers hiddenLayers)
  theta2  <- timed "generating theta2" $ (CPU.run <$> gentheta2 hiddenLayers outputLayers)

  let training      = CPU.run1 $ A.lift . fmincg (nnCostFunction inputLayers hiddenLayers outputLayers regularisation (bias (A.use trainImages)) (A.use trainLabels))
      prediction    = CPU.run1 $ predict (A.use weights1) (A.use weights2)
      --
      thetaIn       = CPU.run  $ A.flatten (A.use theta1) A.++ A.flatten (A.use theta2)
      (thetaOut, _) = training thetaIn
      skip          = A.arraySize (A.arrayShape theta1)
      weights1      = CPU.run $ A.reshape (A.constant (A.arrayShape theta1)) (A.take (A.constant skip) (A.use thetaOut))
      weights2      = CPU.run $ A.reshape (A.constant (A.arrayShape theta2)) (A.drop (A.constant skip) (A.use thetaOut))
      testImages'   = CPU.run $ bias (A.use testImages)

  _ <- timed (printf "training network: %d hidden layers, %d training images" hiddenLayers trainSamples) (return $!! thetaOut)
  r <- timed (printf "testing network: %d sample images" testSamples) (return $!! prediction testImages')

  let accuracy      = CPU.run $ testAccuracy (A.use r) (A.use testLabels)
  printf "network accuracy: %.2f%%\n" (100 * (A.indexArray accuracy Z))

  -- defaultMain
  --   [ bgroup "nnCostFunction"
  --     [ bench "train"   $ whnf training thetaIn
  --     , bench "predict" $ whnf prediction testImages'
  --     ]
  --   ]

  return ()


-- Helper functions
-- ----------------

bias :: A.Num a => Acc (Array DIM2 a) -> Acc (Array DIM2 a)
bias arr = ones A.++ arr
  where
    Z :. h :. _ = A.unlift (A.shape arr)  :: Z :. Exp Int :. Exp Int
    ones        = A.fill (A.index2 h 1) 1

loadImages' :: FilePath -> IO (Array DIM2 Float)
loadImages' file = do
  arr <- loadImages file
  let Z :. imgs :. h :. w = A.arrayShape arr
      arr'                = A.map A.fromIntegral (A.reshape (A.constant (Z :. imgs :. w*h)) (A.use arr))
  return $!! CPU.run arr'

loadLabels' :: FilePath -> IO (Array DIM1 Float)
loadLabels' file = do
  arr     <- loadLabels file
  return $!! CPU.run (A.map A.fromIntegral (A.use arr))


loading :: (FilePath -> IO a) -> FilePath -> IO a
loading with file = timed (printf "loading file '%s'" file) (with file)

timed :: String -> IO a -> IO a
timed label action = do
  printf "%s..." label
  hFlush stdout
  wall0 <- getCurrentTime
  cpu0  <- getCPUTime
  r     <- action
  cpu1  <- getCPUTime
  wall1 <- getCurrentTime
  let wallTime = realToFrac (diffUTCTime wall1 wall0) :: Double
      cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12   :: Double
  printf " done (%s, %.2f x speedup)\n" (A.showFFloatSIBase (Just 3) 1000 wallTime "s") (cpuTime / wallTime)
  return r

