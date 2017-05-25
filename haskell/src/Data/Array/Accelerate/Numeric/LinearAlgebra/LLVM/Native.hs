{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Array.Accelerate.Numeric.LinearAlgebra.LLVM.Native ( mXm )
  where

import Data.Array.Accelerate                                        as A
import Data.Array.Accelerate.IO                                     as A
import Data.Array.Accelerate.Numeric.LinearAlgebra.Type             as A

import Data.Array.Accelerate.LLVM.Native.Foreign

import qualified Numeric.LinearAlgebra                              as N
import qualified Prelude                                            as P


mXm :: forall e. Numeric e => ForeignAcc ((Matrix e, Matrix e) -> Matrix e)
mXm = ForeignAcc "mXm.hmatrix" go
  where
    go (a,b) = P.return $
      case numericR (undefined::e) of
        NumericRfloat  -> toAcc (fromAcc a N.<> fromAcc b)
        NumericRdouble -> toAcc (fromAcc a N.<> fromAcc b)

toAcc :: forall e. Numeric e => N.Matrix e -> A.Matrix e
toAcc m =
  let r = N.rows m
      c = N.cols m
  in case numericR (undefined::e) of
       NumericRfloat  -> fromVectors (Z :. r :. c) (N.flatten m)
       NumericRdouble -> fromVectors (Z :. r :. c) (N.flatten m)


fromAcc :: forall e. Numeric e => A.Matrix e -> N.Matrix e
fromAcc m =
  let Z :. _ :. c = arrayShape m
  in case numericR (undefined::e) of
       NumericRfloat  -> N.reshape c (toVectors m)
       NumericRdouble -> N.reshape c (toVectors m)

