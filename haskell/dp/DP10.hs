module DP where

loeb :: Functor f => f (f b -> b) -> f b
loeb fs = go where go = fmap ($ go) fs

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

make :: Num a => [Int] -> Int -> [[a]] -> [a]
make _  0 = const $ 1 : repeat 0
make cs k = if k == length cs
                then \w -> zipWith (+) (w !! 0)     (replicate (cs !! (k-1)) 0 ++ (w !! k))
                else \w -> zipWith (+) (w !! (k+1)) (replicate (cs !! (k-1)) 0 ++ (w !! k))

blah :: [[Int]]
blah = loeb $ map (make coins) [0..length coins]

main :: IO ()
main = print $ blah !! 1 !! 200
