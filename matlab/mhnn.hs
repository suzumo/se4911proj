{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}

-- random numbers: https://github.com/tmcdonell/mwc-random-accelerate
--
-- linear vector things: https://github.com/tmcdonell/linear-accelerate

-- ask why these imports needed
import Prelude                                    as P
import Debug.Trace
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

test :: IO (Scalar Float)
test = do
    xs <- loadSample
    ys <- loadLabelA
    let Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int
    let theta = generateTheta 401 -- TODO figure out how to get w into this 401 position
    -- let (f1, df1) = lrCostFunction (use theta) xs (use ys) (0.1 :: Exp Float)
    -- do it for i = 1:num_labels
    let lambda = (0.1 :: Exp Float)
    let c = (1.0 :: Exp Float)
    let yc = yEqCFloatVec ys c
    let (jsC, thetaC) = lrCostFunction theta xs yc lambda
    
    -- let (thetaC, jsC) = fmincg theta xs ys c lambda
    -- return $ run (lift (jsC, thetaC))
    -- jsC :: Acc (Scalar Float)
    return $ run (lift jsC)
    -- return $ run (lift thetaC)


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


loadY1 :: IO (Vector Float)
loadY1 = do
    ys <- loadLabelA
    let c = (1.0 :: Exp Float)
    let yc = (yEqCFloatVec ys c)
    return $ run (lift yc)

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


-- TODO?
all_theta :: 
       Acc (Matrix Float)               -- X (data matrix)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- lambda (learning rate)s
    -> Acc (Matrix Float)               -- result theta matrix
all_theta xs ys lambda = undefined


-- to complete
nnCostFunction ::
       Acc (Matrix Float)               -- input theta matrix (25x401) -input_num 400
    -> Acc (Matrix Float)               -- hidden theta matrix (10x26) -hidden_num 25
    -> Exp Int                          -- num of labels
    -> Acc (Matrix Float)               -- X (data matrix) in the form [1 X] (5000x401)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- lambda
    -> ( Acc (Scalar Float)             -- J (cost)
       , Acc (Vector Float) )           -- final theta
nnCostFunction theta1 theta2 n xs y lambda = 
    let
        Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int

        toYs :: Acc (Vector Float) -> Acc (Matrix Float)
        toYs y = undefined
        -- make vector y into matrix Y

        ys :: Acc (Matrix Float)
        ys = toYs y

        -- feedforward
        a3 :: Acc (Matrix Float)
        a1 = xs -- (5000x401)
        z2 = mmult a1 (A.transpose theta1) -- (5000x401) x (401x25) = (5000 x 25) 
        a2 = (fill (constant (Z :. 5000 :. 1)) 1 :: Acc (Matrix Float)) A.++ 
             A.map sigmoid z2 -- (5000x26) -- this should be h...
        z3 = mmult a2 (A.transpose theta2) -- (5000x26) x (26x10) = (5000x10)
        a3 = A.map sigmoid z3 -- (5000x10)

        -- calculate cost J
        j :: Acc (Scalar Float)
        j = A.zipWith (+) regCost 
          $ A.sum 
          $ A.map (\x -> x / A.fromIntegral h)
          $ A.sum
          $ A.zipWith (-) fstMat sndMat
          where
            fstMat  = A.zipWith (*) (A.map negate ys) (A.map log a3)
            sndMat  = A.zipWith (\y a -> -y * (log a) - (1-y)*log(1-a)) ys a3
            regCost = A.sum
                    $ A.map (\x -> x * lambda/(2*(A.fromIntegral h)))
                      (A.zipWith (+) j1 j2)
            j1      = foldAll (+) 0 (A.zipWith (*) ttheta1 ttheta1)
            j2      = foldAll (+) 0 (A.zipWith (*) ttheta2 ttheta2)

        ttheta1 = A.tail theta1
        ttheta2 = A.tail theta2

        -- backpropagate to get gradients
        d3 = A.zipWith (-) a3 ys
        d2 = A.zipWith (*) 
             (mmult (transpose theta2) d3)
             ((fill (constant (Z :. 5000 :. 1)) 1 :: Acc (Matrix Float)) A.++ (sigmoidGradient z2))

        theta2grad = A.map (\x -> x/A.fromIntegral h) 
                   $ mmult d3 (transpose a2)
        theta1grad = A.map (\x -> x/A.fromIntegral h) 
                   $ transpose
                   $ mmult (transpose a1) (A.tail (transpose d2))

        -- add gradient regularisation
        theta1grad_ = A.zipWith (+) theta1grad 
                    $ A.map (\x -> lambda * x/A.fromIntegral h)
                      ((fill (constant (Z :. 25 :. 1)) 1 :: Acc (Matrix Float)) A.++ (A.tail theta1))
                      where
                        s = A.size theta1 -- height of array MUST FIX THIS!!!
        
        theta2grad_ = A.zipWith (+) theta2grad 
                    $ A.map (\x -> lambda * x/A.fromIntegral h)
                      ((fill (constant (Z :. 10 :. 1)) 1 :: Acc (Matrix Float)) A.++ (A.tail theta2))
                      where
                        s = A.size theta2 -- height of array MUST FIX THIS!!!

                   -- d2(2:end, :)

                   -- d2 = (100x50) -> d2 = 99x50
                   -- d2' = (50x100) -> transpose (tail (transpose d2) (50x99)) (99x50) * (transpose a1 (50x500)) (99x500)

                   -- tranpose (transpose a1 (500x50) * tail (transpose d2) (50x99))
        
        grads = flatten theta1grad_ A.++ flatten theta2grad_

        -- g = exp(-z) ./ ((1.0 + exp(-z)) .^ 2)
        -- sigmoidGradient can also take in a Vector...
        sigmoidGradient :: Acc (Matrix Float) -> Acc (Matrix Float)
        sigmoidGradient a = A.map (\x -> exp(-x) / (1.0 + exp(-x))P.^2) a

    in
    (j, grads)


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
fmincg theta xs yc c lambda = 
    let
        fX = fill (constant (Z :. (0::Int))) (0 :: Exp Float)
        (f1, df1) = lrCostFunction theta xs ys lambda
        s = A.map negate df1
        d1 = A.map negate $ A.sum (A.zipWith (*) s s)        
        z1 = unit ((1::Exp Float)/(1 - (the d1)))
        ys = yEqCFloatVec yc c
    in
    outerMostLoop theta xs ys s d1 f1 z1 df1 fX lambda


outerMostLoop ::
       Acc (Vector Float) -- theta
    -> Acc (Matrix Float) -- xs
    -> Acc (Vector Float) -- yc
    -> Acc (Vector Float) -- s
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- f1
    -> Acc (Scalar Float) -- z1
    -> Acc (Vector Float) -- df1
    -> Acc (Vector Float) -- fX0
    -> Exp Float          -- lambda
    -> ( Acc (Vector Float)
       , Acc (Vector Float)) -- j, theta for c
outerMostLoop theta0 xs yc s0 d10 f10 z10 df10 fX0 lambda =
    let
        length0 :: Acc (Scalar Int)
        length0 = unit (0 :: Exp Int)

        -- s0 :: Acc (Vector Float)
        -- s0 = A.map A.negate df10

        -- var that exist before loop: f1, fX, s, df1, d1, z1, length
        initial :: Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
        initial = lift (theta0, fX0, s0, df10, f10, d10, z10, length0)

        cond :: Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
            -> Acc (Scalar Bool) 
        cond args = 
            let theta, s, fX, df1 :: Acc (Vector Float)
                f1, d1, z1 :: Acc (Scalar Float)
                length :: Acc (Scalar Int)
                (theta, fX, s, df1, f1, d1, z1, length) = unlift args
            in
            unit ((the length) A.< (10 :: Exp Int)) -- should repeat til length < 50
    
        body :: Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
            -> Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
        body args =
            let
                (theta, fX, s, df1, f1, d1, z1, length) = unlift args :: (Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int))
                -- f0 = f1
                -- df0 = df1
                length_ = A.map (+1) length
                theta1 = A.zipWith (+) theta (A.map ((the z1) A.*) s) -- set up initial values
                (f2, df2) = lrCostFunction theta1 xs yc lambda
                d2 = A.sum $ A.zipWith (*) df2 s
                f3 = f1
                d3 = d1
                z2 = unit (1 :: Exp Float) -- dummy value...
                z3 = A.map negate z1
                m = unit (1 :: Exp Int) -- max M=20
                limit = unit ((-1) :: Exp Float)

                -- call middleLoop...
                (theta', df2', d2', d3', f2', f3', z1', z2', z3') = middleLoop theta1 s df2 d1 d2 d3 f1 f2 f3 z1 z2 z3 xs yc lambda m limit

                -- test if successful or not
                theta_, s_, fX_, df1_, df2_ :: Acc (Vector Float)
                d2_, f1_, d1_, z1_ :: Acc (Scalar Float)
                (theta_, f1_, fX_, s_, df1_, df2_, d1_, d2_, z1_) = unlift finalCal --handleSuccess theta' s d1 f1 f2' z1' df1 df2 fX -- 

                finalCal = condition
                     ?| ( lift $ handleSuccess theta' s d1 f1 f2' z1' df1 df2' fX , 
                          lift $ handleFailure theta s d1 d2 f1 f2' z1' df10 df2' fX )
                
                condition = ((the d2') A.> (0.5 * (the d1))) A.&& (A.not (the f2' A.> (the f1 + (the z1')*0.01*(the d1)) A.|| the d2' A.> (-0.5)*(the d1)))
                
            in 
            lift (theta_, fX_, s_, df1_, f1_, d1_, z1_, length_)

        z1', d1', f1' :: Acc (Scalar Float)
        length' :: Acc (Scalar Int)
        theta', df1', s' :: Acc (Vector Float)
        (theta', fX', s', df1', f1', d1', z1', length') = unlift $ awhile cond body initial
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
        fX_ = fX0 A.++ (reshape (constant (Z:.(1::Int))) f11) -- update cost array?
        s1 = A.zipWith (-) firstBit df20      -- Polack-Ribiere direction
        firstBit = A.map ((dividend/divisor) *) s0
        dividend = the (A.sum (A.zipWith (*) df20 df20)) - the (A.sum (A.zipWith (*) df10 df20))
        divisor = the $ A.sum (A.zipWith (*) df10 df10) 
        df11 = df20
        df21 = df10
        d21 = A.sum (A.zipWith (*) df11 s1)
        s2 = ((the d21) A.> 0) 
            ?| ( A.map negate df11 , s1 ) 
        d22 = ((the d21) A.> 0) 
            ?| ( A.map negate (A.sum (A.zipWith (*) s2 s2)), d21)
        z11 = unit (the z10 * (A.min 100 ((the d10)/(the d22))))
        -- realmin for float  = 1.1755e-38 
        -- realmin for double = 2.225073858507201e-308
        d11 = d22 
    in
    (theta0, f11, fX_, s2, df11, df21, d11, d22, z11)


handleFailure :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- d2
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
handleFailure theta0 s0 d10 d20 f10 f20 z10 df10 df20 fX0 =
    let
        df11 = df20
        df21 = df10
        s1 = A.map negate df11
        d11 = A.map negate $ A.sum (A.zipWith (*) s1 s1)
        z11 = unit (1/(1 - (the d11)))
        -- d20 = A.sum $ A.zipWith (*) df20 s0 -- d2 isn't really needed but yeah... need something
        -- fX = fX0 A.++ (reshape (constant (Z:.(1::Int))) f10) -- update cost array?
    in
    (theta0, f10, fX0, s1, df11, df21, d11, d20, z11)


middleLoop :: 
       Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s == -df1
    -> Acc (Vector Float) -- df2 
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
    -> Acc (Scalar Int)   -- m
    -> Acc (Scalar Float) -- limit
    -> (  Acc (Vector Float) -- new theta
        , Acc (Vector Float) -- new df2
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- d3
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- f3
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) ) -- z3
middleLoop theta0 s0 df20 d10 d20 d30 f10 f20 f30 z10 z20 z30 xs ys lambda m0 limit0 = 
    let
        -- innerWhile initially once
        initial :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        initial =
            let 
                (theta1, df21, d21, f21, z11, z21, z31, m1, limit1) = innerLoop s0 df20 xs ys lambda d10 f10 d20 f20 f30 z10 z20 z30 m0 theta0 limit0
                -- PROBLEM HERE, STUCK
          in
          lift (theta1, df21, d10, d21, d30, f10, f21, f30, z11, z21, z31, m1, limit1)

        -- loop condition
        cond :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Scalar Bool)
        cond args =
          let _theta, df2 :: Acc (Vector Float)
              _d3, _f3, _z2, _z3, _limit :: Acc (Scalar Float)
              (_theta, df2, d1, d2, _d3, f1, f2, _f3, z1, _z2, _z3, m, _limit) = unlift args
          in
          unit $ A.not (the (A.zipWith6 middleLoopCondition f1 f2 z1 d1 d2 m))

        -- loop body (continue while 'cond' evaluates to True)
        body :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        body args =
          let
              df2' :: Acc (Vector Float)
              z2' :: Acc (Scalar Float) -- TLM: unused??
              (theta', df2', d1', d2', d3', f1', f2', f3', z1', z2', z3', m', limit') = unlift args
              m_ = A.map (A.subtract 1) m'

              (theta'', df2'', d2'', d3_, f2'', f3_, z1'', z2'', z3'')           = middleWhileFunction theta' s0 (the d2') (the d3') (the f2') (the f3') (the z1') (the z3') xs ys lambda (the limit')

              (theta_, df2_, d2_, f2_, z1_, z2_, z3_, m__, limit_)                    = innerLoop s0 df2'' xs ys lambda d1' f1' d2'' f2'' f3_ z1'' z2'' z3'' m_ theta'' limit'
          in
          lift (theta_, df2_, d1', d2_, d3_, f1', f2_, f3_, z1_, z2_, z3_, m__, limit_)

        d12, f12, limit2 :: Acc (Scalar Float)
        m2 :: Acc (Scalar Int)
        (theta2, df22, d12, d22, d32, f12, f22, f32, z12, z22, z32, m2, limit2) = unlift $ awhile cond body initial
    in
    (theta2, df22, d22, d32, f22, f32, z12, z22, z32)


middleWhileFunction :: 
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
middleWhileFunction theta0 s d20 d30 f20 f30 z10 z30 xs ys lambda limit = 
    let 
        z21 = cubicExtrapolate d20 d30 f20 f30 z10 z30 limit
        f31 = f20 -- f3 = f2
        d31 = d20 -- d3 = d2 
        z31 = A.negate z21 -- z3 = -z2
        z11 = z10 + z21 -- z1 = z1 + z2
        theta1 = A.zipWith (+) theta0 (A.map (z21*) s) -- X = X + z2*s
        (f21, df21) = lrCostFunction theta1 xs ys lambda -- [f2 df2] = eval(argstr);
        d21 = A.sum $ A.zipWith (*) df21 s -- d2 = df2'*s;
    in
    (theta1, df21, d21, unit d31, f21, unit f31, unit z11, unit z21, unit z31)


-- (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
middleLoopCondition :: Exp Float -- f1
    -> Exp Float -- f2
    -> Exp Float -- z1
    -> Exp Float -- d1
    -> Exp Float -- d2
    -> Exp Int   -- M
    -> Exp Bool
middleLoopCondition f1 f2 z1 d1 d2 m = f A.|| s A.|| t
    where
        -- if f2 > f1+z1*RHO*d1 | d2 > -SIG*d1 | d2 > SIG*d1 | M == 0
        f = f2 A.> (f1 + z1*0.01*d1) A.|| (d2 A.> (-0.5)*d1) -- failure
        s = d2 A.> 0.5*d1 -- failure
        t = (m A.== 0) -- success


innerLoop :: 
       Acc (Vector Float) -- s
    -> Acc (Vector Float) -- df2
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
        , Acc (Vector Float) -- new df2
        , Acc (Scalar Float) -- new d2
        , Acc (Scalar Float) -- new f2
        , Acc (Scalar Float) -- new z1
        , Acc (Scalar Float) -- new z2
        , Acc (Scalar Float) -- new z3
        , Acc (Scalar Int)   -- new m
        , Acc (Scalar Float) ) -- new limit
innerLoop s df2 xs ys lambda d1 f1 d2 f2 f3 z1 z2 z3 m theta limit =
    let
        initial :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        initial = lift (theta, df2, d2, f2, z1, z2, z3, m, limit)
        -- old = lift (theta, d2, f2, f3, z1, z2, z3, m, limit)

        cond :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Scalar Bool)
        cond args =
          let 
              theta0, df20 :: Acc (Vector Float)
              d20, f20, z10, z20, z30, limit0 :: Acc (Scalar Float)
              (theta0, df20, d20, f20, z10, z20, z30, m0, limit0) = unlift args
          in
          unit $ innerLoopCondition (the f1) (the f20) (the z10) (the d1) (the d20) (the m0)


        body :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
             -> Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        body args =
          let
              theta0, df20 :: Acc (Vector Float)
              d20, f20, z10, z20, z30, limit0 :: Acc (Scalar Float)
              m0 :: Acc (Scalar Int)
              (theta0, df20, d20, f20, z10, z20, z30, m0, limit0) = unlift args
              m_ = A.map (A.subtract 1) m0
              (theta_, df2_, d2_, f2_, z1_, z2_, z3_, limit_) =  innerWhileFunction theta0 s (the d1) (the d20) (the d1) (the f1) (the f20) (the f3) (the z10) (the z20) (the z30) xs ys lambda
          in
          -- lift (theta0, df20, d20, f20, z10, z20, z30, m0, limit0)
          lift (theta_, df2_, d2_, f2_, z1_, z2_, z3_, m_, limit_)

        d2', f2', z2', limit' :: Acc (Scalar Float)
        m' :: Acc (Scalar Int)
        (theta', df2', d2', f2', z1', z2', z3', m', limit') = unlift $ awhile cond body initial

    in 
    (theta', df2', d2', f2', z1', z2', z3', m', limit')


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
        z22 = A.max ( A.min z21 (0.1 * z30) ) (0.9 * z30) -- z2 = max(min(z2, INT*z3),(1-INT)*z3)
        z11 = z10 + z22 -- z1 = z1 + z2;
        theta1 = A.zipWith (+) theta0 (A.map (z22 A.*) s) -- X = X + z2*s;
        (f21, df21) = lrCostFunction theta1 xs ys lambda -- [f2 df2] = eval(argstr);
        d21 = A.sum (A.zipWith (*) df21 s) -- d2 = df2'*s;
        z31 = A.subtract z30 z22; -- z3 = z3-z2;
    in
    (theta1, df21, d21, f21, unit z11, unit z22, unit z31, unit limit)
    

-- (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| (d2 A.> -0.5 * d1)
innerLoopCondition :: Exp Float -- f1
    -> Exp Float -- f2
    -> Exp Float -- z1
    -> Exp Float -- d1
    -> Exp Float -- d2
    -> Exp Int   -- m
    -> Exp Bool
innerLoopCondition f1 f2 z1 d1 d2 m = (f A.&& m A.> 0) -- f1 f2 f3 d1 d2 m
    where
        f = f2 A.> (f1 + z1 * 0.01 * d1) A.|| d2 A.> (-0.5) * d1

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
        grad = A.map (\x -> -x / A.fromIntegral m) 
             $ A.zipWith (+) (A.map (lambda *) theta) $ fold (+) 0 (A.zipWith (*) (transpose xs) hy)

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


sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = (det A.< 0 A.|| divisor A.== 0) 
                        ? ( z3/2 , (P.sqrt det - b)/a )
    where
        divisor = z3 + 3*(d2 + d3)
        a = 6*(f2 - f3)/divisor
        b = 3*(f3 - f2) - z3*(d3 + 2*d2)
        det = b*b - a*d2*z3*z3


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = det A.== 0
                         ? ( z3/2 , z3 - (0.5*d3*z3*z3)/det)
    where 
        det = (d3*z3 + f2 - f3)


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
    if det A.< 0 A.|| divisor A.== 0 A.|| A.isNaN z2 A.|| z2 A.< 0 -- A.|| A.isInfinite z2
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
        z2  = -d2 * z3 * z3 / divisor
        divisor = b + sqrt det


infixl 6 .-
(.-) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
(.-) x = A.map (x-)


infixl 7 .*
(.*) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
(.*) x = A.map (x*)
