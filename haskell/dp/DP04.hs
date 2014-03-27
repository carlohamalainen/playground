module DP where

-- ways :: Int -> [Int]

ways0 = 1 : repeat 0

ways1 = zipWith (+) ways2 (replicate 1   0 ++ ways1)

ways2 = zipWith (+) ways3 (replicate 2   0 ++ ways2)

ways3 = zipWith (+) ways4 (replicate 5   0 ++ ways3)

ways4 = zipWith (+) ways5 (replicate 10  0 ++ ways4)

ways5 = zipWith (+) ways6 (replicate 20  0 ++ ways5)

ways6 = zipWith (+) ways7 (replicate 50  0 ++ ways6)

ways7 = zipWith (+) ways8 (replicate 100 0 ++ ways7)

ways8 = zipWith (+) ways0 (replicate 200 0 ++ ways8)
