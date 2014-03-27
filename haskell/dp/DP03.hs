module DP where

-- ways :: Int -> [Int]

ways0 = 1 : repeat 0

ways1 = n
    where c  = 1
          n  = zipWith (+) ways2 (replicate c 0 ++ n)

ways2 = n
    where c  = 2
          n  = zipWith (+) ways3 (replicate c 0 ++ n)

ways3 = n
    where c  = 5
          n  = zipWith (+) ways4 (replicate c 0 ++ n)

ways4 = n
    where c  = 10
          n  = zipWith (+) ways5 (replicate c 0 ++ n)

ways5 = n
    where c  = 20
          n  = zipWith (+) ways6 (replicate c 0 ++ n)

ways6 = n
    where c  = 50
          n  = zipWith (+) ways7 (replicate c 0 ++ n)

ways7 = n
    where c  = 100
          n  = zipWith (+) ways8 (replicate c 0 ++ n)

ways8 = n
    where c  = 200
          n  = zipWith (+) ways0 (replicate c 0 ++ n)
