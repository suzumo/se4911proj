module Main where

import MHNN

import Prelude                                            as P
import Control.DeepSeq

import Data.Array.Accelerate                              as A
import Data.Array.Accelerate.Debug                        as A
import Data.Array.Accelerate.LLVM.Native                  as CPU
-- import Data.Array.Accelerate.LLVM.PTX                     as PTX
-- import Data.Array.Accelerate.Interpreter                  as I   -- slow


main :: IO ()
main = do
  accInit
  let h = 100
      w = 400
  --
  xs <- loadArray "../data/trainsample100.txt" (Z :. h :. w)
  ys <- loadArray "../data/trainlabel100.txt"  (Z :. h)
  --
  let -- Add a column of 1's to the right of 'xs' matrix
      xs'     = use $ fromFunction (Z:.h:.w+1) (\ix@(_:.x) -> if x P.< w then indexArray xs ix else 1)
      ys'     = use ys
      lambda  = 0.1
      n       = 10
      theta   = all_theta xs' ys' n lambda
      result  = checkResult xs' ys' theta
  --
  print (run result)


-- Load a space-delimited file of numbers into an accelerate array of the given
-- size. It is an error if the file does not contain enough data.
--
loadArray :: (Shape sh, Elt e, Read e) => FilePath -> sh -> IO (Array sh e)
loadArray filename sh = do
  content <- readFile filename
  let strarr  = words content
      dbarr   = P.map read strarr
      arr     = A.fromList sh dbarr
  --
  return (force arr)

