{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RebindableSyntax #-}
module Main where
-- random numbers: https://github.com/tmcdonell/mwc-random-accelerate
--
-- linear vector things: https://github.com/tmcdonell/linear-accelerate

import Prelude                                    as P
import Debug.Trace
import Data.Array.Accelerate                      as A
import Data.Array.Accelerate.LLVM.Native          as I
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Debug

import MMult                                      ( mmult )

type Matrix a = Array DIM2 a


-- trying to translate coursera machine learning neural network 
-- into accelerate version...

main = do
    -- train the neural network with 100 randomized training samples
    thetas <- train "data1000.txt" "label1000.txt" 400 25 10 400
    -- get weights
    let (theta1, theta2) = unlift thetas :: (Acc (Matrix Float), Acc (Matrix Float))
    -- load full data set to test the calculations
    xs <- loadxs "data1000.txt" 1000 400
    ys <- loadys "label1000.txt" 1000
    let pred = predict theta1 theta2 xs
    let result = testAccuracy pred ys
    print $ run result


train :: String -> String -> Int -> Int -> Int -> Int -> IO (Acc (Matrix Float, Matrix Float))
train xsFile ysFile inputl hiddenl ss sampleNum = do
    let lambda = (1.0 :: Exp Float)
    let n = 10
    let l1 = constant inputl
    let l2 = constant hiddenl
    xs <- loadxs xsFile sampleNum inputl
    ys <- loadys ysFile sampleNum
    theta1 <- gentheta2 inputl hiddenl -- 25x401
    theta2 <- gentheta2 hiddenl n -- 10x26
    let ts = flatten theta1 A.++ flatten theta2 

    let (thetas, j) = fmincg (\t -> nnCostFunction t l1 l2 (constant n) xs ys lambda) ts

    -- unroll theta1
    let theta1 = reshape (index2 (constant hiddenl) (constant (inputl+1))) 
               $ A.take ((constant hiddenl)*(constant (inputl+1))) thetas
    let theta2 = reshape (index2  (constant ss) (constant (hiddenl+1))) 
               $ A.drop (constant (hiddenl)*(constant (inputl+1))) thetas

    -- let pred = predict theta1 theta2 xs
    -- let result = checkResult2 pred ys

    -- return $ run (lift result)
    return $ lift (theta1, theta2)


predict ::
       Acc (Matrix Float)               -- input theta matrix
    -> Acc (Matrix Float)               -- hidden theta matrix
    -> Acc (Matrix Float)               -- input matrix to predict label of
    -> Acc (Vector Int)
predict theta1 theta2 xs = 
    let
        h1 = A.map sigmoid 
           $ mmult xs (A.transpose theta1)
        
        Z :. m :. n = unlift (shape h1) :: Z :. Exp Int :. Exp Int
        
        h2 = A.map sigmoid 
           $ mmult ((fill (lift (Z:.m:.(constant 1))) 1 :: Acc (Matrix Float)) A.++ h1)
                   (A.transpose theta2)

        getYs :: Acc (Vector Int)
        getYs
          = A.map ((+1) . A.indexHead . A.fst) -- where A.fst in form of Z :. m :. n -> return 'n'
          $ A.fold1 (\x y -> A.snd x A.> A.snd y ? (x , y))
          $ A.indexed h2

    in
    getYs


testAccuracy :: 
       Acc (Vector Int)
    -> Acc (Vector Float)
    -> Acc (Scalar Float)
testAccuracy predict labels = 
    let
        pa = A.map (\x -> (A.fromIntegral x)/(A.fromIntegral $ A.length labels))
           $ A.sum 
           $ A.map boolToInt
           $ A.zipWith (A.==) (A.map (A.round) labels) predict
    in
    pa


loadxs :: String -> Int -> Int -> IO (Acc (Matrix Float))
loadxs filename m n = do
    content <- readFile filename 
    let strarr = words content
    let dbarr = P.map read strarr
    let ones = fill (constant (Z:.m:.1)) 1 :: Acc (Array DIM2 Float)
    let arr = A.use (A.fromList (Z:.m:.n) dbarr)
    let carr = ones A.++ arr 
    return carr


loadys :: String -> Int -> IO (Acc (Vector Float))
loadys filename m = do
    content <- readFile filename
    let strarr = words content
    let dbarr = P.map read strarr
    let arr = A.use $ A.fromList (Z:.m) dbarr
    return arr


gentheta :: Acc (Matrix Float) -> Acc (Vector Float)
gentheta xs = lift $ A.fill (index1 w) 0
    where
        Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int


gentheta2 :: Int -> Int -> IO (Acc (Matrix Float))
gentheta2 sizeIn sizeOut = do
    let n = (sizeIn + 1)* sizeOut
    let epsilon = 0.12 :: Exp Float
    xs <- withSystemRandom $ \gen -> randomArray (uniformR (0,1)) (Z:.sizeOut:.(sizeIn + 1))
    return $ A.map (\x -> x*2*epsilon - epsilon) (A.use xs)


yEqCFloatVec :: Acc (Vector Float) -> Exp Float -> Acc (Vector Float)
yEqCFloatVec ys c = A.map (A.fromIntegral . boolToInt . (c A.==)) ys


-- make vector ys into matrix ys for neural network
matrixfy :: Acc (Vector Float) -> Acc (Matrix Float)
matrixfy ys = 
    let
        n = 10 :: Exp Int
        zeroMat = A.fill (index2 (A.length ys) n) 0
        onesVec  = A.fill (index1 (A.length ys)) 1.0 :: Acc (Vector Float)
    in
    -- update the theta matrix with the newly computed values for this row
    A.permute const zeroMat (\m -> index2 (unindex1 m) (A.round (ys A.!! (unindex1 m)) -1)) onesVec


nnCostFunction ::
       Acc (Vector Float)               -- input flattened thetas vector 
    -> Exp Int                          -- input layer num
    -> Exp Int                          -- hidden layer num
    -> Exp Int                          -- num of labels
    -> Acc (Matrix Float)               -- X (data matrix) in the form [1 X] (5000x401)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- lambda
    -> Acc (Scalar Float, Vector Float ) -- j, theta1+theta2 vector
nnCostFunction ts l1 l2 n xs y lambda = 
    let
        Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int

        -- unroll theta1
        theta1 = reshape (index2 l2 (l1+1)) $ A.take (l2*(l1+1)) ts
        theta2 = reshape (index2  n (l2+1)) $ A.drop (l2*(l1+1)) ts

        -- make vector y into matrix Y
        ys = matrixfy y

        -- feedforward
        a3 :: Acc (Matrix Float)
        a1 = xs 
        z2 = mmult theta1 (transpose a1) 
        a2 = (fill (lift (Z :. h :. constant  1)) 1 :: Acc (Matrix Float)) 
           A.++ (A.transpose $ A.map sigmoid z2) 
        z3 = mmult a2 (A.transpose theta2) 
        a3 = A.map sigmoid z3 

        -- calculate cost J
        j :: Acc (Scalar Float)
        j = A.zipWith (+) regCost 
          $ A.map (\x -> x / A.fromIntegral h)
          $ A.foldAll (+) 0
          $ A.zipWith (\y a -> -y * (log a) - (1-y)*log(1-a)) ys a3
          where
            regCost = A.map (\x -> x * lambda/(2*(A.fromIntegral h)))
                      (A.zipWith (+) j1 j2)
            j1      = foldAll (+) 0 (A.zipWith (*) ttheta1 ttheta1)
            j2      = foldAll (+) 0 (A.zipWith (*) ttheta2 ttheta2)

        ttheta1 = A.tail theta1 
        ttheta2 = A.tail theta2 

        -- backpropagate to get gradients
        d3 = A.zipWith (-) a3 ys
        d2 = A.zipWith (*) 
             (mmult d3 theta2)
                    ((fill (lift (Z :. h :. constant  1)) 1 :: Acc (Matrix Float)) 
                     A.++ (A.transpose $ A.map sigmoidGradient z2)) 

        theta2grad = A.map (\x -> x/A.fromIntegral h) 
                   $ mmult (transpose d3) a2 
        theta1grad = A.map (\x -> x/A.fromIntegral h) 
                   $ mmult (transpose (A.tail d2)) a1 

        -- add gradient regularisation
        theta1grad_ = A.zipWith (+) theta1grad 
                    $ A.map (\x -> lambda * x/A.fromIntegral h)
                      ((fill (lift (Z :. w1 :. constant 1)) 0 :: Acc (Matrix Float))
                       A.++ ttheta1) 
                      where
                        Z :. h1 :. w1 = unlift (shape theta1) :: Z :. Exp Int :. Exp Int 
        
        theta2grad_ = A.zipWith (+) theta2grad 
                    $ A.map (\x -> lambda * x/A.fromIntegral h)
                      ((fill (lift (Z :. w2 :. constant 1)) 0 :: Acc (Matrix Float))
                       A.++ ttheta2) 
                      where
                        Z :. h2 :. w2 = unlift (shape theta2) :: Z :. Exp Int :. Exp Int 
        
        grads = flatten theta1grad_ A.++ flatten theta2grad_

    in
    lift (j, grads)
    -- lift (theta1grad_, theta2grad_)


-- Minimize a continuous differentialble multivariate function
fmincg :: 
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float))
    -> Acc (Vector Float)               -- theta (weight vector)
    -> (Acc (Vector Float), Acc (Vector Float)) -- theta, j 
fmincg costFunction theta = 
    let
        fX = fill (constant (Z :. (0::Int))) (0 :: Exp Float)
        (f1, df1) = unlift $ costFunction theta
        s = A.map negate df1
        d1 = A.map negate $ A.sum (A.zipWith (*) s s)        
        z1 = unit ((1::Exp Float)/(1 - (the d1)))
        -- ys = yEqCFloatVec yc c
    in
    outerMostLoop costFunction theta s d1 f1 z1 df1 fX


outerMostLoop ::
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float))
    -> Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s
    -> Acc (Scalar Float) -- d1
    -> Acc (Scalar Float) -- f1
    -> Acc (Scalar Float) -- z1
    -> Acc (Vector Float) -- df1
    -> Acc (Vector Float) -- fX0
    -> ( Acc (Vector Float)  -- theta
       , Acc (Vector Float)) -- j
outerMostLoop costFunction theta0 s0 d10 f10 z10 df10 fX0 =
    let
        length0 :: Acc (Scalar Int)
        length0 = unit (0 :: Exp Int)

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
            unit ((the length) A.< (50 :: Exp Int)) -- SET LOOP HERE -- matlab = 50
    
        body :: Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
            -> Acc (Vector Float, Vector Float, Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int)
        body args =
            let
                (theta, fX, s, df1, f1, d1, z1, length) = unlift args :: (Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Float), Acc (Scalar Int))
                length_ = A.map (+1) length
                theta1 = A.zipWith (+) theta (A.map ((the z1) A.*) s) -- set up initial values
                (f2, df2) = unlift $ costFunction theta1
                d2 = A.sum $ A.zipWith (*) df2 s
                f3 = f1
                d3 = d1
                z2 = unit (1 :: Exp Float) -- dummy value...
                z3 = A.map negate z1
                m = unit (1 :: Exp Int) -- max M=20
                limit = unit ((-1) :: Exp Float)

                -- call middleLoop...
                (theta', df2', d2', d3', f2', f3', z1', z2', z3') = middleLoop costFunction theta1 s df2 d1 d2 d3 f1 f2 f3 z1 z2 z3 m limit

                -- test if successful or not
                theta_, s_, fX_, df1_, df2_ :: Acc (Vector Float)
                d2_, f1_, d1_, z1_ :: Acc (Scalar Float)
                (theta_, f1_, fX_, s_, df1_, df2_, d1_, d2_, z1_) = handleSuccess theta' s d1 f1 f2' z1' df1 df2' fX
                    -- unlift finalCal
                                   
                -- seems unnecessary to test this condition......?
                -- finalCal = condition
                --          ?| ( lift $ handleSuccess theta' s d1 f1 f2' z1' df1 df2' fX , 
                --               lift $ handleFailure theta s d1 d2 f1 f2' z1' df10 df2' fX )
                
                -- condition = A.not cond1 A.&& cond2
                --     where
                --         cond1 = the f2' A.> (the f1 + (the z1')*0.01*(the d1)) 
                --            A.|| the d2' A.> (-0.5)*(the d1)
                --         cond2 = the d2' A.> 0.5 * (the d1)
                --         cond3 = (the m) A.== 0
                
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
        fX_ = fX0 A.++ (A.fill (constant (Z:.(1::Int))) 0)
        df11 = df20
        df21 = df10
        s1 = A.map negate df11
        d11 = A.map negate $ A.sum (A.zipWith (*) s1 s1)
        z11 = unit (1/(1 - (the d11)))
    in
    (theta0, f10, fX_, s1, df11, df21, d11, d20, z11)


middleLoop :: 
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float))
    -> Acc (Vector Float) -- theta
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
middleLoop costFunction theta0 s0 df20 d10 d20 d30 f10 f20 f30 z10 z20 z30 m0 limit0 = 
    let
        -- innerWhile initially once
        initial :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        initial =
            let 
                (theta1, df21, d21, f21, z11, z21, z31, m1, limit1) = innerLoop costFunction s0 df20 d10 f10 d20 f20 f30 z10 z20 z30 m0 theta0 limit0
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

              (theta'', df2'', d2'', d3_, f2'', f3_, z1'', z2'', z3'') = middleFunction costFunction theta' s0 (the d2') (the d3') (the f2') (the f3') (the z1') (the z3') (the limit')

              (theta_, df2_, d2_, f2_, z1_, z2_, z3_, m__, limit_)                    = innerLoop costFunction s0 df2'' d1' f1' d2'' f2'' f3_ z1'' z2'' z3'' m_ theta'' limit'
          in
          lift (theta_, df2_, d1', d2_, d3_, f1', f2_, f3_, z1_, z2_, z3_, m__, limit_)

        d12, f12, limit2 :: Acc (Scalar Float)
        m2 :: Acc (Scalar Int)
        (theta2, df22, d12, d22, d32, f12, f22, f32, z12, z22, z32, m2, limit2) = unlift $ awhile cond body initial
    in
    (theta2, df22, d22, d32, f22, f32, z12, z22, z32)


middleFunction :: 
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float))
    -> Acc (Vector Float) -- theta
    -> Acc (Vector Float) -- s == -df1
    -> Exp Float          -- d2
    -> Exp Float          -- d3
    -> Exp Float          -- f2
    -> Exp Float          -- f3
    -> Exp Float          -- z1
    -> Exp Float          -- z3
    -- -> Acc (Matrix Float) -- xs
    -- -> Acc (Vector Float) -- ys
    -- -> Exp Float          -- lambda
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
middleFunction costFunction theta0 s d20 d30 f20 f30 z10 z30 limit = 
    let 
        z21 = cubicExtrapolate d20 d30 f20 f30 z10 z30 limit
        f31 = f20 -- f3 = f2
        d31 = d20 -- d3 = d2 
        z31 = A.negate z21 -- z3 = -z2
        z11 = z10 + z21 -- z1 = z1 + z2
        theta1 = A.zipWith (+) theta0 (A.map (z21*) s) -- X = X + z2*s
        (f21, df21) = unlift $ costFunction theta1 -- [f2 df2] = eval(argstr);
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
        t = (m A.== 0) -- failure


innerLoop :: 
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float)) -- costFunction
    -> Acc (Vector Float) -- s
    -> Acc (Vector Float) -- df2
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
innerLoop costFunction s df2 d1 f1 d2 f2 f3 z1 z2 z3 m theta limit =
    let
        initial :: Acc (Vector Float, Vector Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Float, Scalar Int, Scalar Float)
        initial = lift (theta, df2, d2, f2, z1, z2, z3, m, limit)

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
              (theta_, df2_, d2_, f2_, z1_, z2_, z3_, limit_) =  innerFunction costFunction theta0 s (the d1) (the d20) (the d1) (the f1) (the f20) (the f3) (the z10) (the z20) (the z30)
          in
          lift (theta_, df2_, d2_, f2_, z1_, z2_, z3_, m_, limit_)

        d2', f2', z2', limit' :: Acc (Scalar Float)
        m' :: Acc (Scalar Int)
        (theta', df2', d2', f2', z1', z2', z3', m', limit') = unlift $ awhile cond body initial

    in 
    (theta', df2', d2', f2', z1', z2', z3', m', limit')


innerFunction :: 
       (Acc (Vector Float) -> Acc (Scalar Float, Vector Float))
    -> Acc (Vector Float) -- theta
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
    -> (  Acc (Vector Float) -- new theta
        , Acc (Vector Float) -- df21
        , Acc (Scalar Float) -- d2
        , Acc (Scalar Float) -- f2
        , Acc (Scalar Float) -- z1
        , Acc (Scalar Float) -- z2
        , Acc (Scalar Float) -- z3
        , Acc (Scalar Float) ) -- limit
innerFunction costFunction theta0 s d10 d20 d30 f10 f20 f30 z10 z20 z30 = 
    let 
        limit = z10
        z21  = (f20 A.> f10) 
            ? ( quadraticFit d30 f20 f30 z30, cubicFit d20 d30 f20 f30 z30 )
        z22 = A.max ( A.min z21 (0.1 * z30) ) (0.9 * z30) -- z2 = max(min(z2, INT*z3),(1-INT)*z3)
        z11 = z10 + z22 -- z1 = z1 + z2;
        theta1 = A.zipWith (+) theta0 (A.map (z22 A.*) s) -- X = X + z2*s;
        (f21, df21) = unlift $ costFunction theta1  -- [f2 df2] = eval(argstr);
        d21 = A.sum (A.zipWith (*) df21 s) -- d2 = df2'*s;
        z31 = A.subtract z30 z22; -- z3 = z3-z2;
    in
    (theta1, df21, d21, f21, unit z11, unit z22, unit z31, unit limit)
    

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


-- Logistic Regression Functions...
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


all_theta :: 
       Acc (Matrix Float)               -- X (data matrix)
    -> Acc (Vector Float)               -- y (labels data matching X)
    -> Exp Int                          -- number of labels
    -> Exp Float                        -- lambda (learning rate)s
    -> Acc (Matrix Float)               -- result theta matrix
all_theta xs ys n lambda = 
    let
        Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int
    
        -- create empty result matrix (10x401)
        initial :: Acc (Matrix Float, Scalar Int)
        initial = lift ( A.fill (index2 n w) (0 :: Exp Float), unit 0)

        cond :: Acc (Matrix Float, Scalar Int) -> Acc (Scalar Bool)
        cond args = A.map (A.< n) (A.asnd args)

        body :: Acc (Matrix Float, Scalar Int) -> Acc (Matrix Float, Scalar Int)
        body args = 
            let
                mat :: Acc (Matrix Float)
                row ::Acc (Scalar Int)
                (mat, row) = unlift args -- index i.e. 0,1,..
                c = A.map (+1) row     -- value i.e. 1,2,..
                emptytheta = A.fill (index1 w) 0
                yc = yEqCFloatVec ys (A.fromIntegral $ the c)

                -- get theta for yc
                (theta, j) = fmincg (\t -> lift $ lrCostFunction t xs yc lambda) emptytheta

                -- update the theta matrix with the newly computed values for this row
                mat' = A.permute const mat (\col -> index2 (the row) (unindex1 col)) theta

            in
            lift (mat', c)
    in
    A.afst $ A.awhile cond body initial


checkResult :: 
       Acc (Matrix Float)
    -> Acc (Vector Float)
    -> Matrix Float
    -> Acc (Scalar Int)
checkResult xs ys thetas = 
    let
        pa = A.sum 
          $ A.map boolToInt
          $ A.zipWith (A.==) pLabels (A.map (A.round) ys)

        pLabels = A.map ((+1) . A.indexHead . A.fst)
                $ A.fold1 (\x y -> A.snd x A.> A.snd y ? (x , y))
                $ A.indexed 
                $ mmult xs (transpose (A.use thetas))
    in
    pa


-- maths functions
sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))


sigmoidGradient :: Exp Float -> Exp Float
sigmoidGradient z = (sigmoid z) * (1 - sigmoid z)


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = det A.== 0
                         ? ( z3/2 , z3 - (0.5*d3*z3*z3)/det)
    where 
        det = (d3*z3 + f2 - f3)


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = (det A.< 0 A.|| divisor A.== 0) 
                        ? ( z3/2 , (P.sqrt det - b)/a )
    where
        divisor = z3 + 3*(d2 + d3)
        a = 6*(f2 - f3)/divisor
        b = 3*(f3 - f2) - z3*(d3 + 2*d2)
        det = b*b - a*d2*z3*z3


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
    if det A.< 0 A.|| A.isNaN z2 A.|| divisor A.== 0 A.|| z2 A.< 0 -- A.|| A.isInfinite z2
    then if limit A.< 0.5
        then z1 * 2 -- (EXT - 1)
        else (limit - z1)/2
    else if (limit A.> 0.5) A.&& (z2 + z1) A.> limit
        then (limit - z1)/2
    else if (limit A.< -0.5) A.&& (z2 + z1) A.> z1 * 3
        then z1 * 2 -- (EXT-1.0)
    else if (z2 A.< -z3 * 0.1)
        then -z3 * 0.1
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


-- debug functions
caltheta = do
    xs <- loadxs "trainsample100.txt" 100 400
    ys <- loadys "trainlabel100.txt" 100
    let theta = gentheta xs
    let lambda = 0.1 :: Exp Float
    let n = 10 :: Exp Int
    let thetaMat = all_theta xs ys n lambda
    return $ run thetaMat


test1 :: IO (Vector Float)
test1 = do
    xs <- loadxs "trainsample100.txt" 100 400
    ys <- loadys "trainlabel100.txt" 100
    let theta = gentheta xs 
    let lambda = (0.1 :: Exp Float)
    let yc = yEqCFloatVec ys (1.0 :: Exp Float)
    let (newTheta, j) = fmincg (\t -> lift $ lrCostFunction t xs yc lambda) theta
    return $ run (lift j)


loadyc :: Float -> IO (Vector Float)
loadyc val = do
    ys <- loadys "trainlabel100.txt" 100
    let c = constant val
    let yc = (yEqCFloatVec ys c)
    return $ run (lift yc)


loadt1 :: IO (Acc (Matrix Float))
loadt1 = do
    content <- readFile "theta1.txt" -- filename
    let strarr = words content
    let dbarr = P.map read strarr
    -- let ones = fill (constant (Z:.25:.1)) 1 :: Acc (Array DIM2 Float)
    let arr = A.use $ A.fromList (Z:.25:.401) dbarr
    return arr


loadt2 :: IO (Acc (Matrix Float))
loadt2 = do
    content <- readFile "theta2.txt" -- filename
    let strarr = words content
    let dbarr = P.map read strarr
    -- let ones = fill (constant (Z:.25:.1)) 1 :: Acc (Array DIM2 Float)
    let arr = A.use $ A.fromList (Z:.10:.26) dbarr
    return arr