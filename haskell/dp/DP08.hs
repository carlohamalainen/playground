module DP where

-- ways :: Int -> [Int]

blah :: [[Int]]
blah = let fs   = [ const $ 1 : repeat 0
                  , \w -> zipWith (+) (w !! 2) (replicate 1   0 ++ (w !! 1))
                  , \w -> zipWith (+) (w !! 3) (replicate 2   0 ++ (w !! 2))
                  , \w -> zipWith (+) (w !! 4) (replicate 5   0 ++ (w !! 3))
                  , \w -> zipWith (+) (w !! 5) (replicate 10  0 ++ (w !! 4))
                  , \w -> zipWith (+) (w !! 6) (replicate 20  0 ++ (w !! 5))
                  , \w -> zipWith (+) (w !! 7) (replicate 50  0 ++ (w !! 6))
                  , \w -> zipWith (+) (w !! 8) (replicate 100 0 ++ (w !! 7))
                  , \w -> zipWith (+) (w !! 0) (replicate 200 0 ++ (w !! 8))
                  ]
           ways = map ($ ways) fs
           in ways

main :: IO ()
main = print $ take 201 (blah !! 1)
