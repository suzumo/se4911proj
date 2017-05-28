:set -DACCELERATE_LLVM_NATIVE_BACKEND
:reload
:l MHNN.hs
setFlag dump_exec
theta1 <- gentheta2 784 25
theta2 <- gentheta2 25 10
let ts = flatten theta1 A.++ flatten theta2
mnist <- loadCSV "../../data/mnist_train.csv" :: IO (Array DIM2 Float)
let yt = reshape (index1 (constant 5000)) $ A.drop 55000 (A.transpose (A.take 1 (A.use mnist))) :: Acc (Vector Float)
let xt = A.transpose (A.drop 55000 (A.transpose (A.drop 1 (A.use mnist))))

let ys = reshape (index1 (constant 55000)) $ A.take 55000 (A.transpose (A.take 1 (A.use mnist))) :: Acc (Vector Float)
let xs = A.transpose (A.take 55000 (A.transpose (A.drop 1 (A.use mnist))))

let (thetas, j) = fmincg (\t -> nnCostFunction t (constant 784) (constant 25) (constant 10) xs ys (1.0::Exp Float)) ts
