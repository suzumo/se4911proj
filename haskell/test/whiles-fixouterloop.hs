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
    -> Acc (Scalar Int) -- M (max loop count at 50)
    -> Acc (Vector Float) -- theta
    -> (  Acc (Vector Float) -- new theta
        , Acc (Scalar Float) -- new d2
        , Acc (Scalar Float) -- new f2
        , Acc (Scalar Float) -- new z1
        , Acc (Scalar Float) -- new z2
        , Acc (Scalar Float) -- new z3
        , Acc (Scalar Int) ) -- new m
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
                    lift (theta_, d2_, f2_, f3', z1_, z2_, z3_, m_) )
                    -- :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int))
                    -- don't need this anymore because function is complete! (no more scaffolding '')b
                old
    in
    (new^._1, new^._2, new^._3, new^._5, new^._6, new^._7, new^._8)


outerWhile :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s == -df1
    -> Acc (Scalar Float) -- d1 slope
    -> Acc (Scalar Float) -- d2
    -> Acc (Scalar Float) -- d3
    -> Acc (Scalar Float) -- f1 cost
    -> Acc (Scalar Float) -- f2
    -> Acc (Scalar Float) -- f3
    -> Acc (Scalar Float) -- z1
    -> Acc (Scalar Float) -- z2
    -> Acc (Scalar Float) -- z3
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- ys
    -> Exp Float          -- lambda
    -> Acc (Scalar Float) -- limit
    -> Acc (Scalar Int)   -- M (max 50)
    -> (  Acc (Vector Float) -- new theta
        -- , Acc (Vector Float) -- df2 -- FIX THIS
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- d3
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- f3
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
outerWhile theta0 s d10 d20 d30 f10 f20 f30 z10 z20 z30 xs ys lambda limit m0 =
-- because d3 = d1
-- s = -df1
-- limit = z1
    let
        -- innerWhile initially once
        initial :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
        initial =
          let (theta1, d21, f21, z11, z21, z31, m1) = innerLoop s xs ys lambda d10 f10 d20 f20 f30 z10 z20 z30 m0 theta0
          in
          lift (theta1, d10, d21, d30, f10, f21, f30, z11, z21, z31, m1)

        -- loop condition
        cond :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
             -> Acc (Scalar Bool)
        cond args =
          let _theta :: Acc (Vector Float)
              _d3, _f3, _z2, _z3 :: Acc (Scalar Float)
              (_theta,d1,d2,_d3,f1,f2,_f3,z1,_z2,_z3,m) = unlift args
          in
          A.zipWith6 outerLoopCondition f1 f2 z1 d1 d2 m

        -- loop body (continue while 'cond' evaluates to True)
        body :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
             -> Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
        body args =
          let
              z2' :: Acc (Scalar Float) -- TLM: unused??
              (theta', d1', d2', d3', f1', f2', f3', z1', z2', z3', m') = unlift args
              m_                                                        = A.map (A.subtract 1) m'
              (theta'', df2_, d2'', d3_, f2'', f3_, z1'', z2'', z3'')   = outerWhileFunction theta' s (the d2') (the d3') (the f2') (the f3') (the z1') (the z3') xs ys lambda
              (theta_, d2_, f2_, z1_, z2_, z3_, m__)                    = innerLoop s xs ys lambda d1' f1' d2'' f2'' f3_ z1'' z2'' z3'' m_ theta''
          in
          lift (theta_, d1', d2_, d3_, f1', f2_, f3_, z1_, z2_, z3_, m__)

        -- return just the interesting results of the loop.
        -- extra type signatures necessary for things we don't care about due to
        -- use of 'unlift'
        _d1', _f1' :: Acc (Scalar Float)
        _m' :: Acc (Scalar Int)
        (theta', _d1', d2', d3', _f1', f2', f3', z1', z2', z3', _m') = unlift $ awhile cond body initial
    in
    (theta', d2', d3', f2', f3', z1', z2', z3')



outerWhileFunction :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s == -df1
    -> Exp Float          -- d2
    -> Exp Float          -- d3
    -> Exp Float          -- f2
    -> Exp Float          -- f3
    -> Exp Float          -- z1
    -> Exp Float          -- z3
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- ys
    -> Exp Float          -- lambda
    -> (  Acc (Vector Float) -- new theta
        , Acc (Vector Float) -- df21
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- d3
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- f3
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
outerWhileFunction theta0 s d20 d30 f20 f30 z10 z30 xs ys lambda = 
    let 
        (a, b, z21) = cubicExtrapolate d20 d30 f20 f30 z30

    -- do this later -___-;    
    -- if ~isreal(z2) | isnan(z2) | isinf(z2) | z2 < 0   % num prob or wrong sign?
    --     if limit < -0.5                               % if we have no upper limit
    --         z2 = z1 * (EXT-1);                 % the extrapolate the maximum amount
    --     else
    --         z2 = (limit-z1)/2;                                   % otherwise bisect
    -- elseif (limit > -0.5) & (z2+z1 > limit)          % extraplation beyond max?
    --   z2 = (limit-z1)/2;                                               % bisect
    -- elseif (limit < -0.5) & (z2+z1 > z1*EXT)       % extrapolation beyond limit
    --   z2 = z1*(EXT-1.0);                           % set to extrapolation limit
    -- elseif z2 < -z3*INT
    --   z2 = -z3*INT;
    -- elseif (limit > -0.5) & (z2 < (limit-z1)*(1.0-INT))   % too close to limit?
    --   z2 = (limit-z1)*(1.0-INT);
    -- end
        f31 = f20 -- f3 = f2
        d31 = d20 -- d3 = d2 
        z31 = z21 -- z3 = -z2
        z11 = z10 + z21 -- z1 = z1 + z2
        theta1 = A.zipWith (+) theta0 (multScalerVector z21 s) -- X = X + z2*s
        (f21, df21) = lrCostFunction theta0 xs ys lambda -- [f2 df2] = eval(argstr);
        d21 = A.sum $ A.zipWith (*) df21 s -- d2 = df2'*s;
    in
    (theta1, df21, d21, unit d31, f21, unit f31, unit z11, unit z21, unit z31)


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


-- (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
outerLoopCondition :: Exp Float -- f1
    -> Exp Float -- f2
    -> Exp Float -- z1
    -> Exp Float -- d1
    -> Exp Float -- d2
    -> Exp Int   -- M
    -> Exp Bool
outerLoopCondition f1 f2 z1 d1 d2 m = (f A.|| s A.|| t)
    where
        -- if f2 > f1+z1*RHO*d1 | d2 > -SIG*d1 | d2 > SIG*d1 | M == 0
        f = (f2 A.> (f1 + z1 * 0.01 * d1)) -- failure
        s = ((d2 A.> -0.5 * d1) A.|| lift (m A.== 0)) -- failure
        t = (d2 A.> 0.5 * d1) -- success


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = z2
    where
        a = 6*(f2 - f3)/z3 + 3*(d2 + d3)
        b = 3*(f3 - f2) - z3*(d3 + 2*d2)
        z2 = (P.sqrt (b*b - a*d2*z3*z3) - b)/a


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = z3 - (0.5*d3*z3*z3)/(d3*z3 + f2 - f3)


lrCostFunction :: 
       Acc (Vector Float)      -- theta (weight vector)
    -> Acc (Matrix Float)      -- X (data matrix)
    -> Acc (Vector Float)      -- y (labels)
    -> Exp Float               -- lambda (learning rate)
    -> ( Acc (Scalar Float)    -- J (cost)
       , Acc (Vector Float) )  -- gradients
lrCostFunction theta xs ys lambda = 
    let
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
    in
    (unit jreg, grad)


multScalerVector :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
multScalerVector f v = A.zipWith (*) f' v
    where
        f' = A.replicate (lift (Any :. h)) (unit f)
        Z :. h = unlift (shape v) :: Z :. Exp Int


sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))


equalsInInt :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Int)
equalsInInt y c = A.map boolToInt $ A.zipWith (A.==) y c


cubicExtrapolate :: 
       Exp Float      -- d2
    -> Exp Float      -- d3
    -> Exp Float      -- f2
    -> Exp Float      -- f3
    -> Exp Float      -- z2
    -> ( Exp Float    -- A
       , Exp Float    -- B
       , Exp Float )  -- new z2
cubicExtrapolate d2 d3 f2 f3 z3 = (a, b, z2)
    where 
        a  = 6*(f2 - f3)/z3 + 3*(d2 + d3)
        b  = 3*(f3 - f2) - z3*(d3 + 2*d2)
        z2 = (-d2*z3*z3)/(b + P.sqrt (b*b - a*d2*z3*z3))
