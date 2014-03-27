module DP where


-- import qualified Data.MemoCombinators as Memo

-- coins :: [Int]
-- coins = [1, 2, 5, 10, 20, 50, 100, 200]

-- ways :: [Int] -> [Int]


ways :: Int -> [Int]

ways 0 = 1 : repeat 0

ways 1 = n
    where c  = 1
          cs = 2
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 2 = n
    where c  = 2
          cs = 3
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 3 = n
    where c  = 5
          cs = 4
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 4 = n
    where c  = 10
          cs = 5
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 5 = n
    where c  = 20
          cs = 6
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 6 = n
    where c  = 50
          cs = 7
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 7 = n
    where c  = 100
          cs = 8
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways 8 = n
    where c  = 200
          cs = 0
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)
