{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE RebindableSyntax #-}

module Main
where

-- random numbers: https://github.com/tmcdonell/mwc-random-accelerate
--
-- linear vector things: https://github.com/tmcdonell/linear-accelerate

-- ask why these imports needed
import Prelude                                    as P

import Data.Array.Accelerate                      as A
import Data.Array.Accelerate.Interpreter          as I
-- import Data.Array.Accelerate.System.Random.MWC

import MMult                                      ( mmult )


type Matrix a = Array DIM2 a

-- trying to translate coursera machine learning neural network 
-- into accelerate or haskell version...

main = do
    xs <- loadSample
    ys <- loadLabelA
    i <- getLine
    let Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int
    let theta = generateTheta 401 -- TODO figure out how to get w into this 401 position
    -- let (f1, df1) = lrCostFunction (use theta) xs (use ys) (0.1 :: Exp Float)
    -- do it for i = 1:num_labels
    let y1 = A.map (\e -> (e A.== (1.0 :: Exp Float)) ? (1.0 :: Exp Float, 0.0 :: Exp Float)) (use ys)
    -- t1 <- fmincgIO (use theta) xs y1 (0.1 :: Exp Float) (read i::Int)
    -- print $ run $ lift (f1, df1)
    print $ run (lift y1)

-- Robs
sigmoid :: Exp Float -> Exp Float
sigmoid z = 1.0 / (1.0 + exp(-z))

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

loadSampleA :: IO (A.Array DIM2 Float)
loadSampleA = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    print (P.take 100 strarr)
    let dbarr = P.map read strarr
    let arr = A.fromList (Z:.100:.400) dbarr :: A.Array DIM2 Float
    return arr


loadSample :: IO (Acc (Array DIM2 Float))
loadSample = do
    content <- readFile "trainsample100.txt"
    let strarr = words content
    let dbarr = P.map read strarr
    let ones = fill (constant (Z:.100:.1)) 1 :: Acc (Array DIM2 Float)
    let arr = use (A.fromList (Z:.100:.400) dbarr :: A.Array DIM2 Float)
    let carr = ones A.++ arr -- this should create matrix Z:.100:.401
    return carr


loadLabelA :: IO (Vector Float)
loadLabelA = do
    content <- readFile "trainlabel100.txt"
    let strarr = words content
    let dbarr = P.map read strarr
    let arr = A.fromList (Z:.100) dbarr :: Vector Float
    return arr

generateTheta :: Int -> Vector Float
generateTheta n = A.fromList (Z :. n) (P.replicate n 0) :: Vector Float
--  withSystemRandom $ \gen ->
--    randomArray (uniformR (0,1)) (Z :. n)


-- lrCostFunction(theta, X, y, lambda) = [J, grad]
lrCostFunction
    :: Acc (Vector Float)               -- theta (weight vector)
    -> Acc (Matrix Float)               -- X (data matrix)
    -> Acc (Vector Float)               -- y (labels)
    -> Exp Float                        -- lambda (learning rate)
    -> (Acc (Scalar Float), Acc (Vector Float))
lrCostFunction theta xs ys lambda = (unit jreg, grad)  -- lift :: (Acc (Scalar Float), Acc (Vector Float)) -> Acc (Scalar Float, Matrix Float)
  where
    temp :: Acc (Vector Float) -- theta with theta[0] = 0
    temp = (enumFromN (constant (Z:.1)) 0) A.++ (A.tail theta)

    -- grad = (1/m) * (xs' * (h .- y) + lambda * theta)
    grad :: Acc (Vector Float)
    grad = A.map (\x -> x / A.fromIntegral m) 
         $ A.zipWith (+) (multScalerVector lambda theta) $ fold (+) 0 (A.zipWith (*) (transpose xs) hy)

    -- turn (h .- y) into a matrix for grad
    -- multiply vector (h.-y) into a matrix of height w
    hy :: Acc (Matrix Float)
    hy = A.replicate (lift (Z :. w :. All)) (A.zipWith A.subtract hyp ys)

    m :: Exp Int
    m = A.length ys

    -- n :: Exp Int
    -- n = A.length xs

    Z :. h :. w = unlift (shape xs) :: Z :. Exp Int :. Exp Int

    -- learning rate
    -- lambda :: Exp Float
    -- lambda = (0.1 :: Exp Float)

    jreg :: Exp Float
    jreg = the reg + the j

    -- error accumulation?
    -- j = (1/m) * sum(-y.*(log(hypothesis)) - (1-y).*log(1-hypothesis))
    j :: Acc (Scalar Float)
    j = A.map (\x -> x / A.fromIntegral m)
      $ A.sum
      $ A.zipWith (\h y -> -y * (log h) - (1 - y) * log (1 - h)) hyp ys

    reg :: Acc (Scalar Float) -- regularisation constant (lambda/(2*m)*sum(temp.^2))
    reg = A.map (\x -> lambda * x / A.fromIntegral (2*m)) (A.sum (A.zipWith (*) temp temp))

    -- replicate column vector y into a matrix; where the column is replicated
    -- 'w' times across to form the matrix:
    --
    --   y1        y1 y1 y1 ...
    --   y2   ->   y2 y2 y2 ...
    --   ...             ...
    --
    yy :: Acc (Matrix Float)
    yy  = A.replicate (lift (Z :. All :. w)) ys

    -- same but replicate so that every row is the same, and is 'h' rows high
    --
    --   t1 t2 t3  ->  t1 t2 t3 ...
    --                 t1 t2 t3 ...
    --                    ...
    --
    tt :: Acc (Matrix Float)
    tt = A.replicate (lift (Z :. h :. All)) theta

    -- multiply matrix X with vector theta to get new vector theta
    -- hypothesis = sigmoid (X * theta)
    hyp :: Acc (Vector Float) -- h = X * theta
    hyp = A.map (sigmoid) (fold (+) 0 (A.zipWith (*) xs tt))


-- initial_theta = zeros(n + 1, 1);
-- options = optimset('GradObj', 'on', 'MaxIter', 50);
-- for c=1:num_labels,
--     [theta] = fmincg(@(t)(lrCostFunction(t, X, (y == c), lambda)), initial_theta, options);
--     all_theta(c,:) = theta;
-- end
-- i.e. fmincgIO receives lrCostFunction, theta, as arguments
-- fmincgIO
--     :: Acc (Vector Float)               -- theta (weight vector)
--     -> Acc (Matrix Float)               -- X (data matrix)
--     -> Acc (Vector Float)               -- y (labels)
--     -> Exp Float                        -- lambda (learning rate)
--     -> Int                              -- number of repetitions?
--     -> IO (Acc (Scalar Float), Acc (Vector Float)) -- final j, final_theta
-- fmincgIO theta0 xs yc lambda i = do
--     let (f1, df1) = lrCostFunction theta0 xs yc lambda
--     let s = A.map negate df1
--     let d1 = A.map negate (the $ A.sum (A.zipWith (*) df1 df1))
--     let z1 = 1/(1-d1) -- red = 1
--     let theta1 = A.zipWith (+) theta0 (multScalerVector z1 s) -- X = X + z1*s
--     let (f2, df2) = lrCostFunction theta1 xs yc lambda -- [f2 df2] = eval(argstr)
--     let d2 = the (A.sum $ A.zipWith (A.*) df2 s)-- d2 = df2' * s
--     -- f3 = f1; d3 = d1; z3 = -z1;
--     let (theta3, d2'', d3, f2'', f3, z1'', z2', z3', df2') = outerWhile theta1 s d1 d2 d1 (the f1) (the f2) (the f1) z1 (-z1) xs yc lambda z1 -- limit = z1
--     let (s_, d1_, d2_, f1_, z1_, df1_, df2_) = handleSuccess theta3 s d1 (the d2'') d3 (the f1) (the f2'') f3 z1'' z2' z3' xs df1 df2' lambda z1 -- limit = z1
--     -- recursion??
--     if i P.== 0
--         then return (unit f1_, df2_)
--         else (fmincgIO df2_ xs yc lambda (i-1))

-- fmincg(f, X, options, P1, P2, P3, P4, P5) = [X, fX, i]
-- Minimize a continuous differentialble multivariate function
-- f lrCostFunction, X theta, options null (in all essence), fX (?? - don't need?)
-- TODO: first try fmincg as a IO
-- fmincg
--     :: Acc (Vector Float)               -- theta (weight vector)
--     -> Acc (Matrix Float)               -- X (data matrix)
--     -> Acc (Vector Float)               -- y (labels)
--     -> Exp Float                        -- lambda (learning rate)
--     -> Exp Float                        -- c (certain sample 'row')
--     -> Int                          -- repeat factor?
--     -> (Acc (Scalar Float), Acc (Vector Float)) -- final j, final_theta
-- fmincg theta0 xs ys lambda c 1 = (lrCostFunction theta0 xs ys lambda)
-- fmincg theta0 xs ys lambda c i = fmincg df1 xs ys lambda c (i-1)
--   where
--     df1 :: Acc (Vector Float) --theta
--     df1 = P.snd (lrCostFunction theta0 xs ys lambda)

--     f1 :: Acc (Scalar Float) --j
--     f1 = P.fst (lrCostFunction theta0 xs ys lambda)

    -- d1 :: Acc (Scalar Float) 
    -- d1 = negate $ A.sum (A.zipWith (*) df1 df1)-- -s'*s where s = -df1
    
    -- rho :: Exp Float
    -- rho = 0.01 :: Exp Float -- a bunch of constants for line searches
    
    -- sig :: Exp Float
    -- sig = 0.5 :: Exp Float  -- RHO and SIG are the constants in the Wolfe-Powell conditions

    -- int :: Exp Float
    -- int = 0.1 :: Exp Float  -- don't reevaluate within 0.1 of the limit of the current bracket

    -- ext :: Exp Float
    -- ext = 3.0 :: Exp Float  -- extrapolate maximum 3 times the current bracket

    -- max :: Exp Int
    -- max = 20 :: Exp Int   -- max 20 function evaluations per line search

    -- ratio :: Exp Int
    -- ratio = 100 :: Exp Int 

    --innerWhile theta s d1 d2 d3 f1 f2 f3 z1 z2''' z3 xs ys lambda = (theta', d2', f2', z1', z2, z3')


cubicExtrapolate :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> (Exp Float, Exp Float, Exp Float)
cubicExtrapolate d2 d3 f2 f3 z3 = (a, b, z2)
                         where
                             a = 6*(f2 - f3)/z3 + 3*(d2 + d3)
                             b = 3*(f3 - f2) - z3*(d3 + 2*d2)
                             z2 = (-d2*z3*z3)/(b + P.sqrt (b*b - a*d2*z3*z3))


cubicFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
cubicFit d2 d3 f2 f3 z3 = z2
                         where
                             a = 6*(f2 - f3)/z3 + 3*(d2 + d3)
                             b = 3*(f3 - f2) - z3*(d3 + 2*d2)
                             z2 = (P.sqrt (b*b - a*d2*z3*z3) - b)/a


quadraticFit :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
quadraticFit d3 f2 f3 z3 = z3 - (0.5*d3*z3*z3)/(d3*z3 + f2 - f3)


innerWhile :: Acc (Vector Float) -> Acc (Vector Float) -> Exp Float -> Exp Float -> Exp Float -> Exp Float
 -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Acc (Matrix Float) -> Acc (Vector Float) -> Exp Float -> Int  
 -> (Acc (Vector Float), Acc (Scalar Float), Acc (Scalar Float), Exp Float, Exp Float, Exp Float)
innerWhile theta s d1 d2 d3 f1 f2 f3 z1 z3 xs ys lambda 0 = undefined
innerWhile theta s d1 d2 d3 f1 f2 f3 z1 z3 xs ys lambda n = (theta', d2', f2', z1', z2, z3')

    where
        condition = (f2 A.> (f1 + z1 * 0.01 * d1)) A.|| ((d2 A.> -0.5 * d1) A.&& lift (n P.> 0))
    -- if condition == True, then loop, otherwise return/break innerWhile loop
        -- try to use A.awhile to implement this loop
        -- (?) for scalar conditional
        -- (?|) for array conditional
        -- z2'' = (f2 A.> f1)
        --      ? ( quadraticFit d3 f2 f3 z3
        --        , cubicFit ...
        --        )
        z2'' = (f2 A.> f1) 
             ? ( quadraticFit d3 f2 f3 z3, cubicFit d2 d3 f2 f3 z3 )
        z2'  = (z2'' A./= z2'' ) 
             ? ( z3/2, z2'' )
        z2 = A.max (A.min z2' (0.1 * z3)) (0.9 * z3)
        z1' = z1 + z2
        theta' = A.zipWith (+) theta (multScalerVector z2 s)
        (f2', df2') = lrCostFunction theta' xs ys lambda
        d2' = A.sum (A.zipWith (*) df2' s)
        z3' = z3 - z2;
    -- return z2 -- TODO


outerWhile :: Acc (Vector Float) -> Acc (Vector Float) -> Exp Float -> Exp Float -> Exp Float -> Exp Float
 -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Acc (Matrix Float) -> Acc (Vector Float) -> Exp Float -> Exp Float 
 -> (Acc (Vector Float), Acc(Scalar Float), Exp Float, Acc(Scalar Float), Exp Float, Exp Float, Exp Float, Exp Float, Acc (Vector Float))
outerWhile theta s d1 d2 d3 f1 f2 f3 z1 z3 xs ys lambda limit = (theta', d2', d3', f2', f3', z1', z2', z3', df2')
    where
        -- innerWhile here
        (a, b, z2') = cubicExtrapolate d2 d3 f2 f3 z3
        z2'' = z2'
             -- | ((z2 A./= z2 A.|| z2 A.< 0) A.&& limit A.< -0.5) = z1 * 2.0
             -- | ((z2 A./= z2 A.|| z2 A.< 0) A.&& limit A.>= -0.5) = (limit - z1)/2
             -- | (limit A.> -0.5 A.&& z2 + z1 A.> limit) = (limit - z1)/2
             -- | (limit A.< -0.5 A.&& z2 + z1 A.> z1 * 3.0) = z1 * 2.0
             -- | (z2 A.< -z3 * 0.1) = -z3 * 0.1
             -- | (limit A.> -0.5 A.&& z2 A.< (limit -z1) * 0.9) = (limit -z1) * 0.9
             -- | otherwise = z2'
        f3' = f2
        d3' = d2
        z3' = -z2''
        z1' = z1 + z2''
        theta' = A.zipWith (+) theta (multScalerVector z2'' s)
        (f2', df2') = lrCostFunction theta' xs ys lambda
        d2' = A.sum (A.zipWith (*) df2' s)


handleSuccess :: Acc (Vector Float) -> Acc (Vector Float) -> Exp Float -> Exp Float -> Exp Float -> Exp Float
 -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Acc (Matrix Float) -> Acc (Vector Float) -> Acc (Vector Float) -> Exp Float -> Exp Float 
 -> (Acc (Vector Float), Exp Float, Exp Float, Exp Float, Exp Float, Acc (Vector Float), Acc (Vector Float))
handleSuccess theta s d1 d2 d3 f1 f2 f3 z1 z2 z3 xs df1 df2 lambda limit = (s'', d1', d2'', f1', z1', df1', df2')
    where
        f1' = f2
        -- fX = fX' ++ [f1]
        s' = A.zipWith A.subtract (multScalerVector ((the dividend)/(the divisor)) s) df2      -- Polack-Ribiere direction
        dividend = A.sum $ A.zipWith (-) (A.zipWith (*) df2 df2) (A.zipWith (*) df1 df2)
        divisor = A.sum (A.zipWith (*) df1 df1) 
        df1' = df2
        df2' = df1
        d2' = the (A.sum (A.zipWith (*) df1 s))
        s'' = A.map (A.*(-1)) df1'
            -- ( d2' A.> (0 :: Exp Float) ) -- this giving error that s'' is Exp t0
            -- ? ( A.map (A.*(-1)) df1' , s' )
            -- ( multScalerVector (-1 :: Exp Float) df1' , s' )
        d2'' = the $ A.sum (A.zipWith (*) s' s')
            -- ((the d2') A.> (0 :: Exp Float)) -- this giving error that d2'' is Acc(Scalar Float) not Exp Float
             -- ? ( A.sum (A.zipWith (*) s' s') , d2' )
        -- (s'', d2'') = ((the d2') A.> (0 :: Exp Float))
        --             ? ( (s_, d_) , (s', d2') )
        --                 where
        --                     s_ = multScalerVector (-1 :: Exp Float) df1'
        --                     d_ = A.sum (A.zipWith (*) s' s')
        z1' = z1 * (A.min 100 (d1/(A.subtract d2'' 2.225073858507201e-308))) -- realmin == 2.225073858507201e-308
        d1' = d2'' -- to return [X fX i] but no one cares about i?


infixl 6 .-
(.-) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
(.-) x = A.map (x-)

-- infixl 7 .*
-- (.*) :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
-- (.*) x = A.map (x*)


multScalerVector :: Exp Float -> Acc (Vector Float) -> Acc (Vector Float)
multScalerVector f v = A.zipWith (*) f' v
    where
        f' = A.replicate (lift (Any :. h)) (unit f)
        Z :. h = unlift (shape v) :: Z :. Exp Int


-- negateVector :: Acc (Vector Float) -> Acc (Vector Float)
-- negateVector f = A.map (A.* (-1 :: Exp Float)) f


-- negateScalar :: Exp Float -> Exp Float
-- negateScalar s = (*) (-1 :: Exp Float) s