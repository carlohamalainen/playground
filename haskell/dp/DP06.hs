module DP where

-- ways :: Int -> [Int]

blah :: [[Int]]
blah = let ways0 = 1 : repeat 0
           ways1 = zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
           ways2 = zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
           ways3 = zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
           ways4 = zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
           ways5 = zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
           ways6 = zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
           ways7 = zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
           ways8 = zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
           ways = [ways0, ways1, ways2, ways3, ways4, ways5, ways6, ways7, ways8]
           in ways

main :: IO ()
main = print $ take 201 (blah !! 1)
