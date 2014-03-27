module DP where

-- ways :: Int -> [Int]

blah :: [[Int]]
blah = let ways = [ 1 : repeat 0
                  , zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
                  , zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
                  , zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
                  , zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
                  , zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
                  , zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
                  , zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
                  , zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
                  ]
           in ways

main :: IO ()
main = print $ take 201 (blah !! 1)
