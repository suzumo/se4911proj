{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE RebindableSyntax #-}

-- random numbers: https://github.com/tmcdonell/mwc-random-accelerate
--
-- linear vector things: https://github.com/tmcdonell/linear-accelerate

-- ask why these imports needed
import Prelude                                    as P

import Data.Array.Accelerate                      as A
import Data.Array.Accelerate.Interpreter          as I
-- import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Control.Lens

import MMult                                      ( mmult )

type Matrix a = Array DIM2 a

innerWhileFunction :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s == -df1
    -> Exp Float          -- slope d1
    -> Exp Float          -- d2
    -> Exp Float          -- d3
    -> Exp Float          -- cost f1
    -> Exp Float          -- f2
    -> Exp Float          -- f3
    -> Exp Float          -- z1
    -> Exp Float          -- z2
    -> Exp Float          -- z3
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- ys
    -> Exp Float          -- lambda
    -> (  Acc (Vector Float) -- new theta
        , Acc (Vector Float) -- df21
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
innerWhileFunction theta0 s d10 d20 d30 f10 f20 f30 z10 z20 z30 xs ys lambda = 
    let 
-- because d3 = d1
-- s = -df1
-- limit = z1
        z21  = (f20 A.> f10) 
            ? ( quadraticFit d30 f20 f30 z30, cubicFit d20 d30 f20 f30 z30 )
        z22 = (z21 A./= z21) -- if isNaN (z2) | isInf (z2)
            ? ( z30/2, z21 )   -- z2 = z3/2
        z23 = A.max (A.min z22 (0.1 * z30)) (0.9 * z30) -- z2 = max(min(z2, INT*z3),(1-INT)*z3)
        z11 = z10 + z23 -- z1 = z1 + z2;
        theta1 = A.zipWith (+) theta0 (multScalerVector z23 s) -- X = X + z2*s;
        (f21, df21) = lrCostFunction theta1 xs ys lambda -- [f2 df2] = eval(argstr);
        d21 = A.sum (A.zipWith (*) df21 s) -- d2 = df2'*s;
        z31 = A.subtract z30 z23; -- z3 = z3-z2;
    in
    (theta1, df21, d21, f21, unit z11, unit z23, unit z31)


innerLoop :: 
       Acc (Vector Float) -- s
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- ys
    -> Exp Float          -- lambda 
    -> Acc (Scalar Float) -- Exp Float -- d1
    -> Acc (Scalar Float) -- Exp Float -- f1 
    -> Acc (Scalar Float) -- d2 -- changes from here
    -> Acc (Scalar Float) -- f2
    -> Acc (Scalar Float) -- f3
    -> Acc (Scalar Float) -- z1
    -> Acc (Scalar Float) -- z2
    -> Acc (Scalar Float) -- z3
    -> Acc (Scalar Int) -- M (max loop count at 20)
    -> Acc (Vector Float) -- theta
    -> (  Acc (Vector Float) -- new theta
        , Acc (Scalar Float) -- new d2
        , Acc (Scalar Float) -- new f2
        , Acc (Scalar Float) -- new z1
        , Acc (Scalar Float) -- new z2
        , Acc (Scalar Float) ) -- new z3
innerLoop s xs ys lambda d1 f1 d2 f2 f3 z1 z2 z3 m theta =
    let
        old = lift (theta, d2, f2, f3, z1, z2, z3, m) 
        new = awhile
                (\args -> A.zipWith6 innerLoopCondition f1 (args^._3) (args^._4) d1 (args^._2) (args^._8))
                (\args ->
                    let 
                        (theta', d2', f2', f3', z1', z2', z3', m') = unlift args :: (Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int))
                        m_ = A.map (A.subtract 1) m'
                        (theta_, df2_, d2_, f2_, z1_, z2_, z3_) =  innerWhileFunction theta' s (the d1) (the d2') (the d1) (the f1) (the f2') (the f3') (the z1') (the z2) (the z3') xs ys lambda
                    in 
                    lift (theta_, d2_, f2_, f3', z1_, z2_, z3_, m_) 
                    -- :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int))
                    -- don't need this anymore because function is complete! (no more scaffolding '')b
                old

                -- f1 f2 z1 d1 d2 m
    in
    (new^._1, new^._2, new^._3, new^._5, new^._6, new^._7)


-- -- awhile :: Arrays a  
-- --     => (Acc a -> Acc (Scalar Bool)) --keep evaluating while this returns True
-- --     -> (Acc a -> Acc a)  -- function to apply
-- --     -> Acc a -- initial value
-- --     -> Acc a
-- --
-- -- (\x -> x) :: a -> a
--     awhile 
--         (\args -> A.zipWith6 innerLoopCondition ()
--         -- loop condition, prev:
--         -- (\args ->
--         --     let 
--         --         (theta', d2', f2', f3', z1', z2', z3', m') = unlift args :: ( Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int) )
--         --     in
--         --     A.zipWith6 innerLoopCondition f1 f2' z1' d1 d2' m')


--         -- (\args -> -- args = Acc a, ignoring it?
--         --     let 
--         --         -- (f1, f2, z1, d1, d2, m) = unlift args
--         --         m' = m - 1
--         --         f = (f2 A.> (f1 + z1 * 0.01 * d1))
--         --         s = ((d2 A.> -0.5 * d1) A.&& lift (m' A.> 0)) 
--         --     in
--         --     unit (f A.|| s) )
--         -- loop body
--         undefined
--         -- (\args -> -- this is the first argument in (Acc a -> Acc a), i.e. Acc a
--         --     let 
--         --         (theta', d2', f2', f3', z1', z2', z3', m') = unlift args
--         --         m_ = A.map (A.subtract 1) m'
--         --         (theta_, df2_, d2_, f2_, z1_, z2_, z3_) =
--         --             innerWhileFunction theta' s d1 d2' d1 f1 f2' f3' z1' z3' xs ys lambda
--         --     in 
--         --     lift (theta_, d2_, f2_, f3', z1_, z2_, z3_, m_)) -- CHECK IF CORRECT
--         undefined -- $ lift (theta, d2, f2, f3, z1, z2, z3, m) -- initial values
    

-- (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
innerLoopCondition :: Exp Float -- f1
    -> Exp Float -- f2
    -> Exp Float -- z1
    -> Exp Float -- d1
    -> Exp Float -- d2
    -> Exp Int   -- M
    -> Exp Bool
innerLoopCondition f1 f2 z1 d1 d2 m = (f A.|| s)
    where
        -- (f1, f2, z1, d1, d2, m) = unlift args
        f = (f2 A.> (f1 + z1 * 0.01 * d1))
        s = ((d2 A.> -0.5 * d1) A.&& lift (m A.> 0)) 


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = z2
                         where
                             a = 6*(f2 - f3)/z3 + 3*(d2 + d3)
                             b = 3*(f3 - f2) - z3*(d3 + 2*d2)
                             z2 = (P.sqrt (b*b - a*d2*z3*z3) - b)/a


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = z3 - (0.5*d3*z3*z3)/(d3*z3 + f2 - f3)

lrCostFunction
    :: Acc (Vector Float)               -- theta (weight vector)
    -> Acc (Matrix Float)               -- X (data matrix)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- lambda (learning rate)
    -> (Acc (Scalar Float), Acc (Vector Float))
lrCostFunction theta xs ys lambda = (unit jreg, grad) 
  where
    temp :: Acc (Vector Float) -- theta with theta[0] = 0
    temp = (enumFromN (constant (Z:.1)) 0) A.++ (A.tail theta)

    grad :: Acc (Vector Float)
    grad = A.map (\x -> x / A.fromIntegral m) 
         $ A.zipWith (+) (multScalerVector lambda theta) $ fold (+) 0 (A.zipWith (*) (transpose xs) hy)

    hy :: Acc (Matrix Float)
    hy = A.replicate (lift (Z :. w :. All)) (A.zipWith A.subtract hyp ys)

    m :: Exp Int
    m = A.length ys

    Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int

    jreg :: Exp Float
    jreg = the reg + the j

    j :: Acc (Scalar Float)
    j = A.map (\x -> x / A.fromIntegral m)
      $ A.sum
      $ A.zipWith (\h y -> -y * (log h) - (1 - y) * log (1 - h)) hyp ys

    reg :: Acc (Scalar Float) -- regularisation constant (lambda/(2*m)*sum(temp.^2))
    reg = A.map (\x -> lambda * x / A.fromIntegral (2*m)) (A.sum (A.zipWith (*) temp temp))

    yy :: Acc (Matrix Float)
    yy  = A.replicate (lift (Z :. All :. w)) ys

    tt :: Acc (Matrix Float)
    tt = A.replicate (lift (Z :. h :. All)) theta

    hyp :: Acc (Vector Float) -- h = X * theta
    hyp = A.map (sigmoid) (fold (+) 0 (A.zipWith (*) xs tt))

multScalerVector :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
multScalerVector f v = A.zipWith (*) f' v
    where
        f' = A.replicate (lift (Any :. h)) (unit f)
        Z :. h = unlift (shape v) :: Z :. Exp Int

sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))
