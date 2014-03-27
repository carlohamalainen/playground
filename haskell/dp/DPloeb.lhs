<p> Note to self: solving a dynamic programming problem using <a href="http://carlo-hamalainen.net/blog/2013/11/28/note-to-self-loeb-and-moeb">löb</a>. </p>

> module DPloeb where

<p> The first solution is taken from <a href="http://blog.mno2.org/blog/2011/11/28/writing-dynamic-programming-in-haskell">http://blog.mno2.org/blog/2011/11/28/writing-dynamic-programming-in-haskell</a>. </p>

> coins :: [Int]
> coins = [1, 2, 5, 10, 20, 50, 100, 200]
>
> sol1 :: Int -> Int
> sol1 = (!!) (ways2 coins)
>     where ways1 [] = 1 : repeat 0
>           ways1 (c:cs) = n
>               where n = zipWith (+) (ways1 cs) (replicate c 0 ++ n)
>
> main1 :: IO ()
> main1 = print $ sol1 200

<p> There are 73682 ways to select coins of the denominations specified
in <code>coins</code> that sum to 200: </p>

< *DPloeb> main1
< 73682

<p> First step: roll out the parameter to <code>way</code>: </p>

> ways2 :: [Int] -> [Int]
> ways2 [] = 1 : repeat 0
>
> ways2 [1, 2, 5, 10, 20, 50, 100, 200] = n
>     where c  = 1
>           cs = [2, 5, 10, 20, 50, 100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [2, 5, 10, 20, 50, 100, 200] = n
>     where c  = 2
>           cs = [5, 10, 20, 50, 100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [5, 10, 20, 50, 100, 200] = n
>     where c  = 5
>           cs = [10, 20, 50, 100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [10, 20, 50, 100, 200] = n
>     where c  = 10
>           cs = [20, 50, 100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [20, 50, 100, 200] = n
>     where c  = 20
>           cs = [50, 100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [50, 100, 200] = n
>     where c  = 50
>           cs = [100, 200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [100, 200] = n
>     where c  = 100
>           cs = [200]
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> ways2 [200] = n
>     where c  = 200
>           cs = []
>           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
>
> sol2 :: Int -> Int
> sol2 = (!!) (ways2 [1,2,5,10,20,50,100,200])
>
> main2 :: IO ()
> main2 = print $ sol2 200

<p> Next, change the parameter from <code>[Int]</code> to <code>Int</code> by indexing on integers instead of various lists: </p>

> ways3 :: Int -> [Int]
>
> ways3 0 = 1 : repeat 0
>
> ways3 1 = n
>     where c  = 1
>           cs = 2
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 2 = n
>     where c  = 2
>           cs = 3
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 3 = n
>     where c  = 5
>           cs = 4
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 4 = n
>     where c  = 10
>           cs = 5
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 5 = n
>     where c  = 20
>           cs = 6
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 6 = n
>     where c  = 50
>           cs = 7
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 7 = n
>     where c  = 100
>           cs = 8
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> ways3 8 = n
>     where c  = 200
>           cs = 0
>           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
>
> sol3 :: Int -> Int
> sol3 x = ways3 1 !! x
>
> main3 :: IO ()
> main3 = print $ sol3 200

<p> Substitute the value of <code>cs</code> in the body of the <code>where</code> clauses: </p>

> sol4 :: [Int]
> sol4 = ways1
>   where
>
>   ways0 = 1 : repeat 0
>
>   ways1 = n
>       where c  = 1
>             n  = zipWith (+) ways2 (replicate c 0 ++ n)
>
>   ways2 = n
>       where c  = 2
>             n  = zipWith (+) ways3 (replicate c 0 ++ n)
>
>   ways3 = n
>       where c  = 5
>             n  = zipWith (+) ways4 (replicate c 0 ++ n)
>
>   ways4 = n
>       where c  = 10
>             n  = zipWith (+) ways5 (replicate c 0 ++ n)
>
>   ways5 = n
>       where c  = 20
>             n  = zipWith (+) ways6 (replicate c 0 ++ n)
>
>   ways6 = n
>       where c  = 50
>             n  = zipWith (+) ways7 (replicate c 0 ++ n)
>
>   ways7 = n
>       where c  = 100
>             n  = zipWith (+) ways8 (replicate c 0 ++ n)
>
>   ways8 = n
>       where c  = 200
>             n  = zipWith (+) ways0 (replicate c 0 ++ n)
>
> main4 :: IO ()
> main4 = print $ sol4 !! 200

<p> Substitute the value of <code>c</code> in the body of the <code>where</code> clauses: </p>

> sol5 :: [Int]
> sol5 = ways1
>   where
>   ways0 = 1 : repeat 0
>
>   ways1 = zipWith (+) ways2 (replicate 1   0 ++ ways1)
>   ways2 = zipWith (+) ways3 (replicate 2   0 ++ ways2)
>   ways3 = zipWith (+) ways4 (replicate 5   0 ++ ways3)
>   ways4 = zipWith (+) ways5 (replicate 10  0 ++ ways4)
>   ways5 = zipWith (+) ways6 (replicate 20  0 ++ ways5)
>   ways6 = zipWith (+) ways7 (replicate 50  0 ++ ways6)
>   ways7 = zipWith (+) ways8 (replicate 100 0 ++ ways7)
>   ways8 = zipWith (+) ways0 (replicate 200 0 ++ ways8)
>
> main5 :: IO ()
> main5 = print $ sol5 !! 200

<p> Now tweak the type and collect each of the clauses into a list: </p>

> ways6 :: [[Int]]
> ways6 = let ways0 = 1 : repeat 0
>             ways1 = zipWith (+) ways2 (replicate 1   0 ++ ways1)
>             ways2 = zipWith (+) ways3 (replicate 2   0 ++ ways2)
>             ways3 = zipWith (+) ways4 (replicate 5   0 ++ ways3)
>             ways4 = zipWith (+) ways5 (replicate 10  0 ++ ways4)
>             ways5 = zipWith (+) ways6 (replicate 20  0 ++ ways5)
>             ways6 = zipWith (+) ways7 (replicate 50  0 ++ ways6)
>             ways7 = zipWith (+) ways8 (replicate 100 0 ++ ways7)
>             ways8 = zipWith (+) ways0 (replicate 200 0 ++ ways8)
>             ways = [ways0, ways1, ways2, ways3, ways4, ways5, ways6, ways7, ways8]
>             in ways
>
> sol6 :: Int -> Int
> sol6 x = ways6 !! 1 !! x
>
> main6 :: IO ()
> main6 = print $ sol6 200

<p> Instead of referring to <code>whys4</code>, index into the list with <code>why !! 4</code>: </p>

> ways7 :: [[Int]]
> ways7 = let ways0 = 1 : repeat 0
>             ways1 = zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
>             ways2 = zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
>             ways3 = zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
>             ways4 = zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
>             ways5 = zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
>             ways6 = zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
>             ways7 = zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
>             ways8 = zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
>             ways = [ways0, ways1, ways2, ways3, ways4, ways5, ways6, ways7, ways8]
>             in ways
>
> sol7 :: Int -> Int
> sol7 x = ways7 !! 1 !! x
>
> main7 :: IO ()
> main7 = print $ sol7 200

<p> Now we can define <code>ways</code> directly as a list: </p>

> ways8 :: [[Int]]
> ways8 = let ways = [ 1 : repeat 0
>                    , zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
>                    , zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
>                    , zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
>                    , zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
>                    , zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
>                    , zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
>                    , zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
>                    , zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
>                    ]
>             in ways
>
> sol8 :: Int -> Int
> sol8 x = ways8 !! 1 !! x
>
> main8 :: IO ()
> main8 = print $ sol8 200

<p> Factor out <code>whys</code> by writing each list element as a function. </p>

> ways9 :: [[Int]]
> ways9 = let fs   = [ const $ 1 : repeat 0
>                    , \w -> zipWith (+) (w !! 2) (replicate 1   0 ++ (w !! 1))
>                    , \w -> zipWith (+) (w !! 3) (replicate 2   0 ++ (w !! 2))
>                    , \w -> zipWith (+) (w !! 4) (replicate 5   0 ++ (w !! 3))
>                    , \w -> zipWith (+) (w !! 5) (replicate 10  0 ++ (w !! 4))
>                    , \w -> zipWith (+) (w !! 6) (replicate 20  0 ++ (w !! 5))
>                    , \w -> zipWith (+) (w !! 7) (replicate 50  0 ++ (w !! 6))
>                    , \w -> zipWith (+) (w !! 8) (replicate 100 0 ++ (w !! 7))
>                    , \w -> zipWith (+) (w !! 0) (replicate 200 0 ++ (w !! 8))
>                    ]
>             ways = map ($ ways) fs
>             in ways
>
> sol9 :: Int -> Int
> sol9 x = ways9 !! 1 !! x
>
> main9 :: IO ()
> main9 = print $ sol9 200

<p> Now use <code>loeb</code>: </p>

> loeb :: Functor f => f (f b -> b) -> f b
> loeb fs = go where go = fmap ($ go) fs
>
> fs10 :: [[[Int]] -> [Int]]
> fs10   = [ const $ 1 : repeat 0
>          , \w -> zipWith (+) (w !! 2) (replicate 1   0 ++ (w !! 1))
>          , \w -> zipWith (+) (w !! 3) (replicate 2   0 ++ (w !! 2))
>          , \w -> zipWith (+) (w !! 4) (replicate 5   0 ++ (w !! 3))
>          , \w -> zipWith (+) (w !! 5) (replicate 10  0 ++ (w !! 4))
>          , \w -> zipWith (+) (w !! 6) (replicate 20  0 ++ (w !! 5))
>          , \w -> zipWith (+) (w !! 7) (replicate 50  0 ++ (w !! 6))
>          , \w -> zipWith (+) (w !! 8) (replicate 100 0 ++ (w !! 7))
>          , \w -> zipWith (+) (w !! 0) (replicate 200 0 ++ (w !! 8))
>          ]
>
> sol10 :: Int -> Int
> sol10 x = loeb fs10 !! 1 !! x
>
> main10 :: IO ()
> main10 = print $ sol10 200

<p> Finally, we can generalise this solution by writing a function to
produce the elements of <code>fs10</code>: </p>

> make :: [Int] -> Int -> ([[Int]] -> [Int])
> make _  0 = const $ 1 : repeat 0
> make cs k = if k == length cs
>                 then \w -> zipWith (+) (w !! 0)     (replicate (cs !! (k-1)) 0 ++ (w !! k))
>                 else \w -> zipWith (+) (w !! (k+1)) (replicate (cs !! (k-1)) 0 ++ (w !! k))
>
> sol11 :: Int -> Int
> sol11 x = result !! 1 !! x
>   where result = loeb $ map (make coins) [0..length coins]
>
> main11 :: IO ()
> main11 = print $ sol11 200


<p> These all say 73682: </p>

> mains :: IO ()
> mains = do main1
>            main2
>            main3
>            main4
>            main5
>            main6
>            main7
>            main8
>            main9
>            main10
>            main11
