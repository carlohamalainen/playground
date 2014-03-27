-- http://blog.mno2.org/blog/2011/11/28/writing-dynamic-programming-in-haskell/

coins = [1,2,5,10,20,50,100,200]

sol3 = (!!) (ways [1,2,5,10,20,50,100,200])
    where ways [] = 1 : repeat 0
          ways (coin:coins) = n
              where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

main = do
    print $ sol3 200

