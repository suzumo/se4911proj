{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Array.Accelerate.Numeric.LinearAlgebra (

  Matrix,

  -- * Products
  -- ** Matrix-matrix
  (<>),

) where

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


-- | Dense matrix product
--
-- >>> let a = fromList (Z :. 3 :. 5) [1..]
-- >>> a
-- Matrix (Z:.3:.5)
--  [  1.0,  2.0,  3.0,  4.0,  5.0
--  ,  6.0,  7.0,  8.0,  9.0, 10.0
--  , 11.0, 12.0, 13.0, 14.0, 15.0 ]
--
-- >>> let b = fromList (Z :. 5 :. 2) [1,3, 0,2, -1,5, 7,7, 6,0]
-- >>> b
-- Matrix (Z :. 5 :. 2)
--  [  1.0, 3.0
--  ,  0.0, 2.0
--  , -1.0, 5.0
--  ,  7.0, 7.0
--  ,  6.0, 0.0 ]
--
-- >>> a <> b
-- Matrix (Z :. 3 :. 2)
--  [  56.0,  50.0
--  , 121.0, 135.0
--  , 186.0, 220.0 ]
--
infixr 8 <>
(<>) :: Numeric e => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
(<>) = mmult


-- General dense matrix-matrix multiply written in pure Accelerate. This is not
-- efficient due to the memory access patterns.
--
mmult :: Num e => Acc (Matrix e) -> Acc (Matrix e) -> Acc (Matrix e)
mmult arr brr
  = fold (+) 0
  $ zipWith (*) arrRepl brrRepl
  where
    Z :. rowsA :. _     = unlift (shape arr)  :: Z :. Exp Int :. Exp Int
    Z :. _     :. colsB = unlift (shape brr)  :: Z :. Exp Int :. Exp Int
    --
    arrRepl             = replicate (lift $ Z :. All   :. colsB :. All) arr
    brrRepl             = replicate (lift $ Z :. rowsA :. All   :. All) (transpose brr)


{--
add = A.zipWith (+)
sub = A.zipWith (-)

-- Divide and conquer algorithm for potentially non-square matrices. Bottoms out
-- to `mmult` which could be CUBLAS gemm or naive Accelerate implementation.
--
divconq :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
divconq a b =
    let v = max' n m1 p
    in acond (v A.== n)
       (let (a1,a2) = splitHoriz a
            a1b     = mmult a1 b
            a2b     = mmult a2 b
        in concatHoriz a1b a2b)
       (acond (v A.== p)
        (let (b1,b2) = splitVert b
             ab1 = mmult a b1
             ab2 = mmult a b2
         in concatVert ab1 ab2)
        (let (a1,a2) = splitVert a
             (b1,b2) = splitHoriz b
             a1b1 = mmult a1 b1
             a2b2 = mmult a2 b2
         in add a1b1 a2b2))
    where Z :. n  :. m1 = unlift (shape a)
          Z :. (m2 :: Exp Int) :. p  = unlift (shape b)


-- More sophisticated algorithm, assumes square power-of-2 matrices.
--
strassen :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
strassen a b = concatFour c11 c12 c21 c22
    where (a11,a12,a21,a22) = splitFour a
          (b11,b12,b21,b22) = splitFour b
          m1 = mmult (a11 `add` a22) (b11 `add` b22)
          m2 = mmult (a21 `add` a22) b11
          m3 = mmult a11 (b12 `sub` b22)
          m4 = mmult a22 (b21 `sub` b11)
          m5 = mmult (a11 `add` a12) b22
          m6 = mmult (a21 `sub` a11) (b11 `add` b12)
          m7 = mmult (a12 `sub` a22) (b21 `add` b22)
          c11 = m1 `add` m4 `sub` m5 `add` m7
          c12 = m3 `add` m5
          c21 = m2 `add` m4
          c22 = m1 `sub` m2 `add` m3 `add` m6


max' :: Exp Int -> Exp Int -> Exp Int -> Exp Int
max' a b c =
    cond (a A.> b)
             (cond (a A.> c) a c)
             (cond (b A.> c) b c)

splitHoriz :: Acc (Matrix Double) -> (Acc (Matrix Double), Acc (Matrix Double))
splitHoriz arr = (generate sh1 f1, generate sh2 f2)
    where f1 i = arr ! i
          f2 i = arr ! adjust i
          adjust i =
              let Z :. (m1 :: Exp Int) :. (n1 :: Exp Int) = unlift (shape arr)
                  Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                  m = mi + (m1 `div` (constant 2))
              in lift $ Z :. m :. ni
          sh1 = let Z :. m1 :. n1 = unlift (shape arr)
                in lift $ Z :. (m1 `div` ((constant 2) :: Exp Int)) :. (n1 :: Exp Int)
          sh2 = let Z :. m2 :. n2 = unlift (shape arr)
                in lift $ Z :. (m2 `div` ((constant 2) :: Exp Int)) :. (n2 :: Exp Int)

splitVert :: Acc (Matrix Double) -> (Acc (Matrix Double), Acc (Matrix Double))
splitVert arr = (generate sh1 f1, generate sh2 f2)
    where f1 i = arr ! i
          f2 i = arr ! adjust i
          adjust i =
              let Z :. (m1 :: Exp Int) :. (n1 :: Exp Int) = unlift (shape arr)
                  Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                  n = ni + (n1 `div` (constant 2))
              in lift $ Z :. mi :. n
          sh1 = let Z :. m1 :. n1 = unlift (shape arr)
                in lift $ Z :. (m1 :: Exp Int) :. (n1 `div` ((constant 2) :: Exp Int))
          sh2 = let Z :. m2 :. n2 = unlift (shape arr)
                in lift $ Z :. (m2 :: Exp Int) :. (n2 `div` ((constant 2) :: Exp Int))

splitFour :: Acc (Matrix Double)
          -> (Acc (Matrix Double), Acc (Matrix Double), Acc (Matrix Double), Acc (Matrix Double))
splitFour arr = (generate sh f1, generate sh f2, generate sh f3, generate sh f4)
    where f1 i = arr ! i
          f2 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi :. ni + (n `div` (constant 2))
                 in arr ! i'
          f3 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi + (m `div` (constant 2)) :. ni
                 in arr ! i'
          f4 i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                     i' = lift $ Z :. mi + (m `div` (constant 2)) :. ni + (n `div` (constant 2))
                 in arr ! i'
          sh = let Z :. m :. n = unlift (shape arr)
               in lift $ Z :. (m `div` (constant 2)) :. (n `div` (constant 2))
          Z :. (m :: Exp Int) :. (n :: Exp Int) = unlift (shape arr)

concatVert :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
concatVert = (A.++)

concatHoriz :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
concatHoriz a b = generate sh f
    where sh = lift $ Z :. (ma + mb) :. na
          f i = let Z :. (mi :: Exp Int) :. (ni :: Exp Int) = unlift i
                in lift (mi A.< ma) ?
                       ((a ! i),
                        (b ! (lift $ Z :. (mi - ma) :. ni)))
          Z :. (ma :: Exp Int) :. (na :: Exp Int) = unlift (shape a)
          Z :. (mb :: Exp Int) :. (nb :: Exp Int) = unlift (shape b)

-- (a b)
-- (c d)
concatFour
    :: Acc (Matrix Double) -> Acc (Matrix Double)
    -> Acc (Matrix Double) -> Acc (Matrix Double)
    -> Acc (Matrix Double)
concatFour a b c d =
  concatVert (concatHoriz a c) (concatHoriz b d)
--}

