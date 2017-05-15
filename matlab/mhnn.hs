{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}

-- random numbers: https://github.com/tmcdonell/mwc-random-accelerate
--
-- linear vector things: https://github.com/tmcdonell/linear-accelerate

-- ask why these imports needed
import Prelude                                    as P

import Data.Array.Accelerate                      as A
import Data.Array.Accelerate.Interpreter          as I
-- import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Debug

import MMult                                      ( mmult )

type Matrix a = Array DIM2 a


-- trying to translate coursera machine learning neural network 
-- into accelerate or haskell version...


main = do
    xs <- loadSample
    ys <- loadLabelA
    let Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int
    let theta = generateTheta 401 -- TODO figure out how to get w into this 401 position
    -- let (f1, df1) = lrCostFunction (use theta) xs (use ys) (0.1 :: Exp Float)
    -- do it for i = 1:num_labels
    let lambda = (0.1 :: Exp Float)
    let c = (1.0 :: Exp Float)
    let (thetaC, jsC) = fmincg theta xs ys c lambda
    -- print $ run $ lift (f1, df1)
    print $ run (lift thetaC)
    print $ run (lift jsC)


--parseFile :: String -> Int -> Int -> IO (Array (Int, Int) Int)
--parseFile filename rows cols = do
--    matrix <- liftM readWords $ readFile filename
--    return $ listArray ((1,1), (rows, cols)) matrix

-- loadDataMatrix :: IO ([Float])
-- loadDataMatrix = do
--     content <- readFile "trainsample100.txt"
--     let strarr = words content
--     let dbarr = readWords strarr
--     return dbarr


loadSampleA :: IO (Acc (Matrix Float))
loadSampleA = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    print (P.take 100 strarr)
    let dbarr = P.map read strarr
    let arr = A.use $ A.fromList (Z:.100:.400) dbarr
    return arr


loadSample :: IO (Acc (Matrix Float))
loadSample = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    let dbarr = P.map read strarr
    let ones = fill (constant (Z:.100:.1)) 1 :: Acc (Array DIM2 Float)
    let arr = A.use (A.fromList (Z:.100:.400) dbarr)
    let carr = ones A.++ arr -- this should create matrix Z:.100:.401
    return carr


loadLabelA :: IO (Acc (Vector Float))
loadLabelA = do
    content <- readFile "trainlabel100.txt"
    let strarr = words content
    let dbarr = P.map read strarr
    let arr = A.use $ A.fromList (Z:.100) dbarr
    return arr


generateTheta :: Int -> Acc (Vector Float)
generateTheta n = A.use $ A.fromList (Z :. n) (P.replicate n 0)
--  withSystemRandom $ \gen ->
--    randomArray (uniformR (0,1)) (Z :. n)


-- fmincg(f, X, options, P1, P2, P3, P4, P5) = [X, fX, i]
-- Minimize a continuous differentialble multivariate function
-- f lrCostFunction, X theta, options null (in all essence), fX (?? - don't need?)
fmincg :: 
       Acc (Vector Float)               -- theta (weight vector)
    -> Acc (Matrix Float)               -- X (data matrix)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- c (certain identification class)
    -> Exp Float                        -- lambda (learning rate)s
    -> (Acc (Vector Float), Acc (Vector Float)) -- j, theta for c 
fmincg theta xs ys c lambda = 
    let
        fX0 = fill (constant (Z :. 0)) (0 :: Exp Float)
        (f10, df10) = lrCostFunction theta xs yc lambda
        s0 = A.map negate df10
        d10 = A.map negate $ A.sum (A.zipWith (*) s0 s0)        
        z10 = unit ((1::Exp Float)/(1 - (the d10)))
        yc = yEqCFloatVec ys c
    in
    outerMostLoop theta xs yc d10 f10 z10 df10 s0 fX0 lambda


outerMostLoop ::
       Acc (Vector Float) -- theta
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- yc
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- f1
    -> Acc (Scalar Float) -- z1
    -> Acc (Vector Float) -- df1
    -> Acc (Vector Float) -- s
    -> Acc (Vector Float) -- fX0
    -> Exp Float          -- lambda
    -> ( Acc (Vector Float)
       , Acc (Vector Float)) -- j, theta for c
outerMostLoop theta0 xs yc d10 f10 z10 df10 s0 fX0 lambda =
    let
        length0 :: Acc (Scalar Int)
        length0 = unit (0 :: Exp Int)

        s0 :: Acc (Vector Float)
        s0 = A.map A.negate df10

        -- var that exist before loop: f1, fX, s, df1, d1, z1, length
        initial :: Acc (Vector Float, Scalar Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Int)
        initial = lift (theta0, f10, fX0, s0, df10, d10, z10, length0)

        cond :: Acc (Vector Float, Scalar Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Int)
            -> Acc (Scalar Bool) 
        cond args = 
            let theta, s, fX, df1 :: Acc (Vector Float)
                f1, d1, z1 :: Acc (Scalar Float)
                length :: Acc (Scalar Int)
                (theta, f1, fX, s, df1, d1, z1, length) = unlift args
            in
            unit ((the length) A.< (1 :: Exp Int)) -- should repeat til length < 50
    
        body :: Acc (Vector Float, Scalar Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Int)
            -> Acc (Vector Float, Scalar Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Int)
        body args =
            let
                length_ = A.map (+1) length
                (theta, f1, fX, s, df1, d1, z1, length) = unlift args :: (Acc (Vector Float), Acc (Scalar Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int))
                theta1 = A.zipWith (+) theta (A.map ((the z1) A.*) s) -- set up initial values
                (f2, df2) = lrCostFunction theta1 xs yc lambda
                d2 = A.sum $ A.zipWith (*) df2 s
                f3 = f1
                d3 = d1
                z2 = unit (0 :: Exp Float) -- dummy value...
                z3 = A.map negate z1
                m = unit (0 :: Exp Int) -- max M=20
                limit = unit ((-1) :: Exp Float)

                -- call outerLoop...
                (theta', d2', d3', f2', f3', z1', z2', z3') = outerLoop theta1 s d1 d2 d3 f1 f2 f3 z1 z2 z3 xs yc lambda m
                -- test if successful or not
                theta_, s_, fX_, df1_, df2_ :: Acc (Vector Float)
                d2_, f1_, d1_, z1_ :: Acc (Scalar Float)
                (theta_, f1_, fX_, s_, df1_, df2_, d1_, d2_, z1_) = unlift condition
                condition = (the d2') A.> (0.5 * (the d1))
                     ?| (a, b)
                     where 
                        a = lift $ handleSuccess theta' s d1 f1 f2' z1' df1 df2 fX
                        b = lift $ handleFailure theta s d1 f1 f2' z1' df1 df2 fX
            in
            lift (theta_, f1_, fX_, s_, df1_, d1_, z1_, length_)

        z1', d1', f1' :: Acc (Scalar Float)
        length' :: Acc (Scalar Int)
        theta', df1', s' :: Acc (Vector Float)
        (theta', f1', fX', s', df1', d1', z1', length') = unlift $ awhile cond body initial
    in
    (theta', fX') 


handleSuccess :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- f1
    -> Acc (Scalar Float) -- f2
    -> Acc (Scalar Float) -- z1
    -> Acc (Vector Float) -- df1
    -> Acc (Vector Float) -- df2
    -> Acc (Vector Float) -- fX (array of costs) not actually used?
    -> ( Acc (Vector Float) -- old theta
       , Acc (Scalar Float) -- f1
       , Acc (Vector Float) -- fX (accum cost array?)
       , Acc (Vector Float) -- s
       , Acc (Vector Float) -- df1
       , Acc (Vector Float) -- df2
       , Acc (Scalar Float) -- d1
       , Acc (Scalar Float) -- d2
       , Acc (Scalar Float) ) -- z1
handleSuccess theta0 s0 d10 f10 f20 z10 df10 df20 fX0 =
    let
        f11 = f20
        fX_ = fX0 A.++ (reshape (constant (Z:.(1::Int))) f10) -- update cost array?
        s1 = A.zipWith A.subtract (multScalerVector ((the dividend)/(the divisor)) s0) df20      -- Polack-Ribiere direction
        dividend = A.sum $ A.zipWith (-) (A.zipWith (*) df20 df20) (A.zipWith (*) df10 df20)
        divisor = A.sum (A.zipWith (*) df10 df10) 
        df11 = df20
        df21 = df10
        d21 = A.sum (A.zipWith (*) df11 s1)
        s2 = ((the d21) A.> 0) 
            ?| ( A.map negate df11 , s1 ) 
        d22 = ((the d21) A.> 0) 
            ?| ( A.map negate (A.sum (A.zipWith (*) s2 s2)), d21)
        z11 = A.zipWith (*) z10 (unit (A.min 100 ((the d10)/(A.subtract (the d22) 2.225073858507201e-308)))) -- realmin == 2.225073858507201e-308
        d11 = d22 
    in
    (theta0, f11, fX_, s2, df11, df21, d11, d22, z11)


handleFailure :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- f1
    -> Acc (Scalar Float) -- f2
    -> Acc (Scalar Float) -- z1
    -> Acc (Vector Float) -- df1
    -> Acc (Vector Float) -- df2
    -> Acc (Vector Float) -- fX (array of costs) not actually used?
    -> ( Acc (Vector Float) -- old theta
       , Acc (Scalar Float) -- f1
       , Acc (Vector Float) -- fX (accum cost array?)
       , Acc (Vector Float) -- s
       , Acc (Vector Float) -- df1
       , Acc (Vector Float) -- df2
       , Acc (Scalar Float) -- d1
       , Acc (Scalar Float) -- d2
       , Acc (Scalar Float) ) -- z1
handleFailure theta0 s d10 f10 f20 z10 df10 df20 fX0 =
    let
        d1 = A.map negate $ A.sum (A.zipWith (*) s s)
        z1 = unit (1/(1 A.- (the d1)))
        d2 = A.sum $ A.zipWith (*) df20 s -- d2 isn't really needed but yeah... need something
        fX = fX0 A.++ (reshape (constant (Z:.(1::Int))) f10) -- update cost array?
    in
    (theta0, f10, fX, s, df20, df10, d1, d2, z1)


outerLoop :: 
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
    -> Acc (Scalar Int)   -- length 50
    -> (  Acc (Vector Float) -- new theta
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- d3
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- f3
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
outerLoop theta0 s d10 d20 d30 f10 f20 f30 z10 z20 z30 xs ys lambda m0 = 
    let
        -- innerWhile initially once
        initial :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        initial =
            let 
                (theta1, d21, f21, z11, z21, z31, m1, limit1) = innerLoop s xs ys lambda d10 f10 d20 f20 f30 z10 z20 z30 m0 theta0 limit0
                limit0 = unit (-1 :: Exp Float)
          in
          lift (theta1, d10, d21, d30, f10, f21, f30, z11, z21, z31, m1, limit1)

        -- loop condition
        cond :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Scalar Bool)
        cond args =
          let _theta:: Acc (Vector Float)
              _d3, _f3, _z2, _z3, _limit :: Acc (Scalar Float)
              (_theta, d1, d2, _d3, f1, f2, _f3, z1, _z2, _z3, m, _limit) = unlift args
          in
          A.zipWith6 outerLoopCondition f1 f2 z1 d1 d2 m        

        -- loop body (continue while 'cond' evaluates to True)
        body :: Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        body args =
          let
              z2' :: Acc (Scalar Float) -- TLM: unused??
              (theta', d1', d2', d3', f1', f2', f3', z1', z2', z3', m', limit') = unlift args
              m_                                                                = A.map (A.subtract 1) m'
              (theta'', df2_, d2'', d3_, f2'', f3_, z1'', z2'', z3'')           = outerWhileFunction theta' s (the d2') (the d3') (the f2') (the f3') (the z1') (the z3') xs ys lambda (the limit')
              (theta_, d2_, f2_, z1_, z2_, z3_, m__, limit_)                    = innerLoop s xs ys lambda d1' f1' d2'' f2'' f3_ z1'' z2'' z3'' m_ theta'' limit'
          in
          lift (theta_, d1', d2_, d3_, f1', f2_, f3_, z1_, z2_, z3_, m__, limit_)

        _d1', _f1', _limit' :: Acc (Scalar Float)
        _m' :: Acc (Scalar Int)
        (theta', _d1', d2', d3', _f1', f2', f3', z1', z2', z3', _m', _limit') = unlift $ awhile cond body initial
    in
    (theta', d2', d3', f2', f3', z1', z2', z3')


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
    -> Acc (Scalar Float) -- limit
    -> (  Acc (Vector Float) -- new theta
        , Acc (Scalar Float) -- new d2
        , Acc (Scalar Float) -- new f2
        , Acc (Scalar Float) -- new z1
        , Acc (Scalar Float) -- new z2
        , Acc (Scalar Float) -- new z3
        , Acc (Scalar Int)   -- new m
        , Acc (Scalar Float) ) -- new limit
innerLoop s xs ys lambda d1 f1 d2 f2 f3 z1 z2 z3 m theta limit =
    let
        old = lift (theta, d2, f2, f3, z1, z2, z3, m, limit)
        new = awhile
                (\args -> A.zipWith6 innerLoopCondition f1 (args^._3) (args^._4) d1 (args^._2) (args^._8))
                (\args ->
                    let 
                        (theta', d2', f2', f3', z1', z2', z3', m', limit') = unlift args :: (Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int), Acc (Scalar Float))
                        m_ = A.map (A.subtract 1) m'
                        (theta_, df2_, d2_, f2_, z1_, z2_, z3_, limit_) =  innerWhileFunction theta' s (the d1) (the d2') (the d1) (the f1) (the f2') (the f3') (the z1') (the z2) (the z3') xs ys lambda
                    in 
                    lift (theta_, d2_, f2_, f3', z1_, z2_, z3_, m_, limit_) )
                old
    in
    (new^._1, new^._2, new^._3, new^._5, new^._6, new^._7, new^._8, new^._9)


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
    -> Exp Float          -- limit
    -> (  Acc (Vector Float) -- new theta
        , Acc (Vector Float) -- df21
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- d3
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- f3
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
outerWhileFunction theta0 s d20 d30 f20 f30 z10 z30 xs ys lambda limit = 
    let 
        z21 = cubicExtrapolate d20 d30 f20 f30 z10 z30 limit
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
        , Acc (Scalar Float) -- z3
        , Acc (Scalar Float) ) -- limit
innerWhileFunction theta0 s d10 d20 d30 f10 f20 f30 z10 z20 z30 xs ys lambda = 
    let 
        limit = z10
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
    (theta1, df21, d21, f21, unit z11, unit z23, unit z31, unit limit)
    

-- (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
innerLoopCondition :: Exp Float -- f1
    -> Exp Float -- f2
    -> Exp Float -- z1
    -> Exp Float -- d1
    -> Exp Float -- d2
    -> Exp Int   -- M
    -> Exp Bool
innerLoopCondition f1 f2 z1 d1 d2 m = (f A.&& s)
    where
        f = (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
        s = m A.> 0


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
        f = (f2 A.> (f1 + z1 * 0.01 * d1) A.|| (d2 A.> -0.5 * d1)) -- failure
        s = (d2 A.> 0.5 * d1) -- failure
        t = (m A.== 0) -- success


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = z2
    where
        a = 6*(f2 - f3)/z3 + 3*(d2 + d3)
        b = 3*(f3 - f2) - z3*(d3 + 2*d2)
        z2 = (P.sqrt (b*b - a*d2*z3*z3) - b)/a


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = z3 - (0.5*d3*z3*z3)/(d3*z3 + f2 - f3)


-- lrCostFunction(theta, X, y, lambda) = [J, grad]
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


yEqCFloatVec :: Acc (Vector Float) -> Exp Float -> Acc (Vector Float)
yEqCFloatVec ys c = A.map (A.fromIntegral . boolToInt . (c A.==)) ys


cubicExtrapolate :: 
       Exp Float      -- d2
    -> Exp Float      -- d3
    -> Exp Float      -- f2
    -> Exp Float      -- f3
    -> Exp Float      -- z1
    -> Exp Float      -- z3
    -> Exp Float      -- limit
    -> Exp Float      -- z2
cubicExtrapolate d2 d3 f2 f3 z1 z3 limit =
    if det A.< 0 A.|| A.isNaN z2 A.|| z2 A.< 0 -- A.|| A.isInfinite z2
    then if limit A.< 0.5
        then z1 * (3 - 1)
        else (limit - z1)/2
    else if (limit A.> 0.5) A.&& (z2 + z1) A.> limit
        then (limit - z1)/2
    else if (limit A.< 0.5) A.&& (z2 + z1) A.> z1 * 3
        then z1 * 2 -- (EXT-1.0)
    else if (z2 A.< -z3 * 0.1)
        then -z2 * 0.1
    else if (limit A.> -0.5) A.&& (z2 A.< (limit-z1)*0.9) -- (limit-z1)*(1.0-INT)
        then (limit - z1)*(0.9) -- (1.0 - INT)
    else
        z2
    where 
        a   = 6*(f2 - f3)/z3 + 3*(d2 + d3)
        b   = 3*(f3 - f2) - z3*(d3 + 2*d2)
        det = b*b - a*d2*z3*z3
        z2  = -d2 * z3 * z3 / (b + sqrt det)


infixl 6 .-
(.-) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
(.-) x = A.map (x-)


infixl 7 .*
(.*) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
(.*) x = A.map (x*)
