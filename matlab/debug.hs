-- debug checkResult
xs <- loadxs
ys <- loadys
theta <- caltheta
run $ checkResult xs ys theta





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
