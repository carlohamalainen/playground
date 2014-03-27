module DP where


-- import qualified Data.MemoCombinators as Memo

coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

sol3 :: Int -> Int
sol3 = (!!) (ways coins)

ways :: [Int] -> [Int]
ways [] = 1 : repeat 0

ways [1, 2, 5, 10, 20, 50, 100, 200] = n
    where c  = 1
          cs = [2, 5, 10, 20, 50, 100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [2, 5, 10, 20, 50, 100, 200] = n
    where c  = 2
          cs = [5, 10, 20, 50, 100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [5, 10, 20, 50, 100, 200] = n
    where c  = 5
          cs = [10, 20, 50, 100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [10, 20, 50, 100, 200] = n
    where c  = 10
          cs = [20, 50, 100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [20, 50, 100, 200] = n
    where c  = 20
          cs = [50, 100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [50, 100, 200] = n
    where c  = 50
          cs = [100, 200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [100, 200] = n
    where c  = 100
          cs = [200]
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

ways [200] = n
    where c  = 200
          cs = []
          n  = zipWith (+) (ways cs) (replicate c 0 ++ n)

-- ways (c:cs) = n
-- where n = zipWith (+) (ways cs) (replicate c 0 ++ n)


main :: IO ()
main = print $ sol3 200

