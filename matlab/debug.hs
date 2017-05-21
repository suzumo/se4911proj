-- debug nn
n = 10 :: Int
xs <- loadxs "trainsample100.txt" 100 400 -- 100x401
ys <- loadys "trainlabel100.txt" 100 -- 1x100
ymat = matrixfy ys -- 100x10
theta1 <- loadt1
theta2 <- loadt2
-- theta1 <- gentheta2 400 25 -- 25x401
-- theta2 <- gentheta2 25 n   -- 10x26
lambda = 1 :: Exp Float
l1 = 400 :: Exp Int
l2 = 25 :: Exp Int
ts = (flatten theta1) A.++ (flatten theta2)

result = nnCostFunction ts l1 l2 (constant n) xs ys lambda



-- nnCostFunction
-- X = 100x401
-- m = 100
-- a1 = ones(1:100) ++ 401x100
a1 = xs -- 100x401 <> 401 x 100
z2 = mmult theta1 (transpose a1) -- 25x401 X 401x100 <> 25x401 X 401x100 = 25x100
a2 = (fill (lift (Z :.(constant 100):. constant  1)) 1 :: Acc (Matrix Float)) A.++ (A.transpose $ A.map sigmoid z2) -- 100x26
z3 = mmult a2 (A.transpose theta2) -- 100x10
a3 = A.map sigmoid z3 -- 100x10

ttheta1 = A.tail theta1 -- 25x400
ttheta2 = A.tail theta2 -- 10x25

j1 = foldAll (+) 0 (A.zipWith (*) ttheta1 ttheta1)
j2 = foldAll (+) 0 (A.zipWith (*) ttheta2 ttheta2)
regCost = A.map (\x -> x * lambda/(2*(A.fromIntegral (100::Exp Int)))) (A.zipWith (+) j1 j2)

j = A.zipWith (+) regCost $ A.map (\x -> x / A.fromIntegral (100::Exp Int)) $ A.foldAll (+) 0 $ A.zipWith (\y a -> -y * (log a) - (1-y)*log(1-a)) ymat a3


d3 = A.zipWith (-) a3 ymat -- 100x10
d2 = A.zipWith (*) (mmult d3 theta2) ((fill (lift (Z :. constant 100 :. constant 1)) 1 :: Acc (Matrix Float)) A.++ (A.transpose $ A.map sigmoidGradient z2)) -- 100x26 .+ 100x26

-- 10x100 X 100x26
theta2grad = A.map (\x -> x/(A.fromIntegral (100::Exp Int))) $ mmult (transpose d3) a2 -- 10x100 X 100x26 = 10X26
theta1grad = A.map (\x -> x/(A.fromIntegral (100::Exp Int))) $ mmult (transpose (A.tail d2)) a1 -- 25x100 X 100X401 = 25x401

theta1grad_ = A.zipWith (+) theta1grad $ A.map (\x -> lambda * x/A.fromIntegral (100::Exp Int)) (fill (constant (Z :. 25 :. 1)) 0 :: Acc (Matrix Float)) A.++ ttheta1 -- should be 25x401
theta2grad_ = A.zipWith (+) theta2grad $ A.map (\x -> lambda * x/A.fromIntegral (100::Exp Int)) (fill (constant (Z :. 10 :. 1)) 0 :: Acc (Matrix Float)) A.++ ttheta2 -- should be 10x26


-- debug checkResult
xs <- loadxs
ys <- loadys
theta <- caltheta
run $ checkResult xs ys theta

-- debug lr
y <- loadY1
lambda = (0.1 :: Exp Float)
ys = A.use y
theta = generateTheta 401
xs <- loadSample
fX = fill (constant (Z :. (0::Int))) (0 :: Exp Float)
costFunction = lrCostFunction xs ys lambda
(f1, df1) = unlift $ costFunction theta
s = A.map negate df1
d1 = A.map negate $ A.sum (A.zipWith (*) s s)        
z1 = unit ((1::Exp Float)/(1 - (the d1)))

length = unit (0 :: Exp Int)
theta1 = A.zipWith (+) theta (A.map ((the z1) A.*) s)
(f2, df2) = lrCostFunction theta1 xs ys lambda
d2 = A.sum $ A.zipWith (*) df2 s
f3 = f1
d3 = d1
z2 = unit (1 :: Exp Float) -- dummy value...
z3 = A.map negate z1
m = unit (2 :: Exp Int) -- max M=20
limit = unit ((-1) :: Exp Float)

-- checking middle loop
(theta', df2', d2', d3', f2', f3', z1', z2', z3') = middleLoop theta1 s df2 d1 d2 d3 f1 f2 f3 z1 z2 z3 xs ys lambda m limit

-- check success/failure condition
condition = ((the d2') A.> (0.5 * (the d1))) A.&& (A.not (the f2' A.> (the f1 + (the z1')*0.01*(the d1)) A.|| the d2' A.> (-0.5)*(the d1)))
run $ unit condition

(theta_, f1_, fX_, s_, df1_, df2_, d1_, d2_, z1_) = handleSuccess theta' s d1 f1 f2' z1' df1 df2' fX






-- checking outerLoop
(thetax, fXx) = outerMostLoop theta xs ys s d1 f1 z1 df1 fX lambda





-- checking inner loop
-- inner loop condition check 
run $ unit $ innerLoopCondition (the f1) (the f2) (the z1) (the d1) (the d2) (the m)
(theta2, df21, d21, f21, z11, z21, z31, m1, limit1) = innerLoop s df2 xs ys lambda d1 f1 d2 f2 f3 z1 z2 z3 m theta1 limit

-- middle loop condition check
run $ unit $ A.not (the (A.zipWith6 middleLoopCondition f1 f21 z11 d1 d21 m)) -- true, exit middle loop

-- check success
