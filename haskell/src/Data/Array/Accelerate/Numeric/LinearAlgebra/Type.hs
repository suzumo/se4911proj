{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Accelerate.Numeric.LinearAlgebra.Type
  where

import Data.Array.Accelerate                              as A


-- | Matrix representation
--
type Matrix a = Array DIM2 a


data NumericR a where
  NumericRfloat   :: NumericR Float
  NumericRdouble  :: NumericR Double

class (Elt a, Num a) => Numeric a where
  numericR :: {- dummy -} a -> NumericR a

instance Numeric Float where
  numericR _ = NumericRfloat

instance Numeric Double where
  numericR _ = NumericRdouble

