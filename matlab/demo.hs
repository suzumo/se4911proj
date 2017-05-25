thetas <- train "data1000.txt" "label1000.txt" 400 25 10 300
let (theta1, theta2) = unlift thetas :: (Acc (Matrix Float), Acc (Matrix Float))

xs <- loadxs "data1000.txt" 1000 400
ys <- loadys "label1000.txt" 1000

let pred = predict theta1 theta2 xs
let result = testAccuracy pred ys
