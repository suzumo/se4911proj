-- how to use permute function

test :: Acc (Vector Float) -> Acc (Matrix Float)
test ys =
  let
      initial :: Acc (Matrix Float, Scalar Int)
      initial = lift ( A.fill (index2 (A.length ys) 10) 0, unit 0 )

      cond :: Acc (Matrix Float, Scalar Int) -> Acc (Scalar Bool)
      cond mi = A.map (A.< A.length ys) (A.asnd mi)

      body :: Acc (Matrix Float, Scalar Int) -> Acc (Matrix Float, Scalar Int)
      body (unlift -> (mat,j)) =
        let
            -- fmincg
            row  = A.map (+ A.fromIntegral (the j)) (A.take 10 ys)

            -- update the theta matrix with the newly computed values for this row
            mat' = A.permute const mat (\i -> index2 (the j) (unindex1 i)) row

            -- next row
            i'   = A.map (+1) j
        in
        lift (mat', i')
  in
  A.afst $ A.awhile cond body initial