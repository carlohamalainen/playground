<p> Working through the detail of 
<a href="https://github.com/quchen/articles/blob/master/loeb-moeb.md">Löb and möb: strange loops in Haskell</a> and 
the related discussion on <a href="http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/">reddit.com/r/haskell</a>, in particular
<a href="http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/cdhsefm">psygnisfive's comment</a>. There's nothing much original in this post, I'm just working through the details. </p>

> module Loeb where

<p> In a spreadsheet one has a set of cells, and each cell can be defined in terms of values
in the other cells. For example, let's use four cells
<code>x0</code>,
<code>x1</code>,
<code>x2</code>, and
<code>x3</code> with the following definition: </p>

> f1 :: [Int]
> f1 = let xs0 = 1
>          xs1 = succ xs0
>          xs2 = succ xs1
>          xs3 = succ xs2
>          xs  = [xs0, xs1, xs2, xs3]
>          in xs


<p> The variable <code>xs0</code> appears in a few places but it can be factored out. 
Remove <code>xs0 = 1</code> and substitute the value <code>1</code> in the definition for <code>xs</code>. Also, <code>xs0</code> is the first element of the <code>xs</code> list, so refer
to it using <code>xs !! 0</code>:</p>

> f2 :: [Int]
> f2 = let xs1 = succ (xs !! 0)
>          xs2 = succ xs1
>          xs3 = succ xs2
>          xs = [1, xs1, xs2, xs3]
>          in xs

<p> Now do the same for <code>xs1</code>:

> f3 :: [Int]
> f3 = let xs2 = succ (xs !! 1)
>          xs3 = succ xs2
>          xs = [1, succ (xs !! 0), xs2, xs3]
>          in xs

<p> The pattern should be clear, so now factor out <code>xs2</code>
and <code>xs3</code>: </p>

> f4 :: [Int]
> f4 = let xs = [ 1
>               , succ (xs !! 0)
>               , succ (xs !! 1)
>               , succ (xs !! 2)
>               ]
>          in xs

<p> The common feature of the last three lines is that they are a function of <code>xs</code>.
The first line is the constant <code>1</code>, and we can make this a function of <code>xs</code>
with something like </p>

< \_ -> 1

<p> but the standard prelude provides <code>const 1</code> for just this purpose. So now we have: </p>

> f4_1 :: [Int]
> f4_1 = let xs = [ const 1 $ xs
>                 , succ (xs !! 0)
>                 , succ (xs !! 1)
>                 , succ (xs !! 2)
>                 ]
>          in xs

<p> So each line is a function of <code>xs</code>. Can we factor it
out, in a sense, so each line looks more like the first? Yes: </p>

> f4_2 :: [Int]
> f4_2 = let xs = [ const 1 $ xs
>                 , succ . (\h -> h !! 0) $ xs
>                 , succ . (\h -> h !! 1) $ xs
>                 , succ . (\h -> h !! 2) $ xs 
>                 ]
>          in xs

<p> The lambda expressions are a bit cumbersome. What we are doing is
the <code>succ</code> function after selecting a certain element of a list. 
Haskell supports <a href="http://www.haskell.org/haskellwiki/Currying">currying</a>, and when
one curries an operator, the left vs right arguments are respected: </p>

<pre>
*Main> :t (!!)
(!!) :: [a] -> Int -> a

*Main> :t (!! 3)
(!! 3) :: [a] -> a
</pre>

<p> So <code>succ (xs !! 0)</code> can be rewritten as <code>succ . (!! 0) $ xs</code>. Here
is the next version: </p>

> f5 :: [Int]
> f5 = let xs = [ const 1       $ xs
>               , succ . (!! 0) $ xs
>               , succ . (!! 1) $ xs
>               , succ . (!! 2) $ xs
>               ]
>         in xs

<p> We can still ask if there is a way to generalise the definition of <code>f5</code>.
Each line is of the form <code> function $ xs</code> so we could define a list of
functions </p>

< fs = [ const 1
<      , succ . (!! 0)
<      , succ . (!! 1)
<      , succ . (!! 2)
<      ]

<p> and then <code>xs = map (\f -> f xs) fs</code>. In full: </p>

> f6_1 :: [Int]
> f6_1 = let fs = [ const 1
>                 , succ . (!! 0)
>                 , succ . (!! 1)
>                 , succ . (!! 2)
>                 ]
>            xs = map (\f -> f xs) fs
>            in xs 

<p> Finally, the lambda expression is a bit clunky and Haskell provides
the dollar-sign operator for function application (which is all the
lambda expression is actually doing). With currying we get an appropriate type: </p>

<pre>
*Main> :t ($)
($) :: (a -> b) -> a -> b

*Main> :t ($ [1, 2, 3])
($ [1, 2, 3]) :: Num t => ([t] -> b) -> b
</pre>

<p> so <code>($ xs)</code> will be a function that takes 
a function that operates on a list and returns something (as long as <code>xs</code> is a list). This is just what we need: </p>

> f6_2 :: [Int]
> f6_2 = let fs = [ const 1
>                 , succ . (!! 0)
>                 , succ . (!! 1)
>                 , succ . (!! 2)
>                 ]
>            xs = map ($ xs) fs
>           in xs 

<p> and this is the final form in <a href="http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/cdhsefm">psygnisfive's</a> comment. </p>

<p> (Embarrassingly for myself, I had assumed that the type of
<code>(!! xs)</code> would be the result of currying on its
<i>left-hand</i> parameter, not the right, which made the <code>map
($ xs) fs</code> form incomprehensible.) </p>


<p> To finish things off, we'd like to write a function that computes the result <code>f6_2</code>
given the list <code>fs</code>. Here's a first attempt: </p>

< loeb1 fs = let xs = map ($ xs) fs in xs

<p> An alternative to using a <code>let</code> definition is to use
a <code>where</code> (this brings us closer to the form given by
<a href="https://github.com/quchen">quchen</a>): </p>

< loeb2 fs = go where go = map ($ go) fs

<p> Looking at the type of <code>loeb2</code>, </p>

< loeb2 :: [[b] -> b] -> [b]

<p> shows the fact that we used a list as the starting point for the derivation. The first parameter
is a list of functions that take a list and produce a value (of the same type), and the result is list.
The final remark in psygnisfive's comment is "rinse and repeat for your favourite functor." What this refers to is the fact that <code>map</code> is specialised for lists. Functors generalise the idea of being able to "map over" something, and <code>fmap</code> generalises <code>map</code>: </p>

<pre>
*Main> :t map
map :: (a -> b) -> [a] -> [b]

*Main> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

*Main> map (+2) [1, 2, 3]
[3,4,5]

*Main> fmap (+2) [1, 2, 3]
[3,4,5]

*Main> :m Data.Maybe

Prelude Data.Maybe> fmap (+1) (Just 3)
Just 4

Prelude Data.Maybe> fmap (+1) Nothing
Nothing
</pre>

<p> Changing <code>map</code> to <code>fmap</code> in the definition of <code>loeb2</code> gets us the actual definition of <code>loeb</code>: </p>

> loeb :: Functor f => f (f b -> b) -> f b
> loeb fs = go where go = fmap ($ go) fs

<p> For what it's worth, putting <code>f = []</code> specialises to the type signature of the earlier
<code>loeb2</code>: </p>

< loeb :: [] ([] b -> b) -> [] b

<p> which can be rewritten in the usual form </p>

< loeb :: [[b] -> b] -> [b]

<p> It doesn't end! You can then abstract out the <code>fmap</code> by making it a parameter, which gives
the <code>moeb</code> function: </p>

> moeb :: t -> (((a -> b) -> b) -> t -> a) -> a
> moeb fs x = go where go = x ($ go) fs

<p> See the <a href="http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/">discussion on reddit</a> for motivation and possible uses of <code>moeb</code>. </p>


