Property based testing is a great way to improve code quality since it captures logical properties of your system.
Instead of writing test cases by hand, you capture logical relationships and then let the test framework generate
hundreds or thousands of examples for you.

For a general introduction to property based testing (language-independent), try
this YOW! Night talk [Property Based Testing Finding More Bugs with Less Effort](https://www.youtube.com/watch?v=hP-VstNdFGo)
by [Charles O'Farrell](https://twitter.com/charlesofarrell).

QuickCheck provides a typeclass ``CoArbitrary`` for generating random *functions*. In this blog post
we derive ``CoArbitrary`` in a standalone setting.  For the real definition, 
see [Test.QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck.html#g:10).

Some imports:

> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE RankNTypes   #-}
>
> module Sample where
> 
> import Control.Applicative
> import Control.Monad
> import System.Random

First, a generator of type ``Gen a`` lets us use a random number generator
to get a value of type ``a``:

> newtype Gen a = Gen
>     { unGen :: StdGen -> a }

(The real ``Gen`` in QuickCheck is parameterised over the generator, but we don't need that here.
Also, the real ``Gen`` includes a size, which you can control
using [resize](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html#v:resize)
or [scale](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html#v:scale)).

The ``Arbitrary`` typeclass:

> class Arbitrary a where
>     arbitrary :: Gen a
>

Since ``False < True`` (there is an instance of ``Ord`` for ``Bool``) we can
use [randomR](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:randomR)
to define ``arbitrary`` for ``Bool``:

> instance Arbitrary Bool where
>     arbitrary = Gen $ \stdgen ->
>       let (r, _) = randomR (False, True) stdgen in r

Here's a first attempt at implementing ``Gen (Bool -> Bool)``, a generator for boolean functions:

> genBoolFn0 :: Gen (Bool -> Bool)
> genBoolFn0 = Gen $ \stdgen ->
>   \a -> let (r, _) = randomR (False, True) stdgen in r

The type is right but it's going to generate pretty boring functions since it doesn't even use the ``a``:

> runBoolFnGen :: Gen (Bool -> Bool) -> IO ()
> runBoolFnGen g = do
>   fns  <- samples g
> 
>   forM_ fns  $ \f -> do
>     putStrLn $ "True  => " ++ show (f True)
>     putStrLn $ "False => " ++ show (f False)
>     putStrLn ""

The functions are either ``const True`` or ``const False``. Not useful.

    ghci> runBoolFnGen genBoolFn0 
    True  => True
    False => True

    True  => False
    False => False

    True  => False
    False => False

    True  => True
    False => True

    True  => False
    False => False

    True  => True
    False => True

    True  => False
    False => False

    True  => False
    False => False

    True  => False
    False => False

We need to split on the ``a`` somehow:

> genBoolFn1 :: Gen (Bool -> Bool)
> genBoolFn1 = Gen $ \stdgen -> \a -> case a of
>   True  -> let (r, _) = randomR (False, True) stdgen in r
>   False -> let (r, _) = randomR (False, True) stdgen in r

This isn't any better. The other thing we can change is the generator. Fortunately, ``StdGen`` is an instance of ``RandomGen``, so we have the [split](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:split)
function:

< -- The split operation allows one to obtain two
< -- distinct random number generators. This is
< -- very useful in functional programs (for example,
< -- when passing a random number generator down to
< -- recursive calls), but very little work has been done
< -- on statistically robust implementations of split
< -- ([System.Random, System.Random] are the only
< -- examples we know of).
<
< split :: StdGen -> (StdGen, StdGen)
< split = ...

Taking advantage of laziness, we can use split to write a pure function
that gives us an infinite sequence of statistically distinct generators:

> splitGenerator :: RandomGen a => a -> [a]
> splitGenerator r = r0 : splitGenerator r1
>   where
>     (r0, r1) = split r

This is exactly what we need to permute the generator in ``genBoolFn1``.
Let's map ``True`` to the generator at index ``0`` and ``False`` to to the generator at index ``1``:

> genBoolFn2 :: Gen (Bool -> Bool)
> genBoolFn2 = Gen $ \stdgen -> \a -> case a of
>   True  -> let (r, _) = randomR
>                           (False, True)
>                           (splitGenerator stdgen !! 0)
>             in r
>   False -> let (r, _) = randomR
>                           (False, True)
>                           (splitGenerator stdgen !! 1)
>             in r

Now the random functions look more random:

    ghci> runBoolFnGen genBoolFn2
    True  => False
    False => True

    True  => True
    False => False

    True  => False
    False => True

    True  => True
    False => False

    True  => True
    False => True

    True  => True
    False => False

    True  => True
    False => True

    True  => False
    False => False

    True  => False
    False => True

So, what about random integer functions ``Int -> Int``? We need to map any integer to one of the split generators, in other words we need a mapping
$\mathbb{N} \rightarrow \mathbb Z_{\ge 0}$. Send the non-negative integers to the non-negative even integers, and the 
negative integers to the positive odd integers:

\[
n \rightarrow
        \begin{cases}
                 2n        & \mbox{if } n \geq 0 \\
                 2(-n) + 1 & \mbox{if } n < 0
        \end{cases}
\]

In Haskell this looks like:

< \n -> variant $ if n >= 0 then 2*n else 2*(-n) + 1

where

> variant :: Int -> Gen b -> Gen b
> variant v (Gen g) = Gen $ \r -> g $ splitGenerator r !! (v+1)

Capture this with a typeclass:

> class CoArbitrary a where
>     coarbitrary :: a -> Gen b -> Gen b
>
> instance CoArbitrary Bool where
>   coarbitrary False = variant 0
>   coarbitrary True  = variant 1
>
> instance CoArbitrary Int where
>     coarbitrary n = variant $ if n >= 0 then 2*n else 2*(-n) + 1

With some experimentation we can extend the ``CoArbitrary`` definitions to lists and tuples:

> instance CoArbitrary a => CoArbitrary [a] where
>   coarbitrary []     = variant 0
>   coarbitrary (x:xs) = variant 1 . coarbitrary (x, xs)
>
> instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (a, b) where
>   coarbitrary (x, y) = coarbitrary x . coarbitrary y

In general, if we have ``CoArbitrary a`` and ``Arbitrary b`` then we can
derive ``Arbitrary (a -> b)``:

> instance (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
>     arbitrary = promote (\a -> coarbitrary a arbitrary)
>
> promote :: (a -> Gen b) -> (Gen (a->b))
> promote f = Gen $ \r ->
>   \a -> let Gen h = f a
>          in h r

Here are more examples of random functions:

> runGenFn :: (Arbitrary a, Arbitrary b, Show a, Show b)
>          => Gen (a -> b)
>          -> [a]
>          -> IO ()
> runGenFn g as = do
>   fns  <- samples g
> 
>   forM_ fns  $ \f -> do
>     forM_ as $ \a -> putStrLn $ show a ++ " => "
>                                        ++ show (f a)
>     putStrLn ""

Running in ghci:

    ghci> runGenFn (arbitrary :: Gen (Int -> Int)) [0, 1, 2]
    0 => 198
    1 => 940
    2 => -200

    0 => 734
    1 => -627
    2 => 6

    0 => 965
    1 => 581
    2 => -585

    0 => -306
    1 => -918
    2 => 678

    0 => -929
    1 => 336
    2 => -696

    0 => -66
    1 => 123
    2 => 875

    0 => -234
    1 => -673
    2 => 216

    0 => 355
    1 => 56
    2 => -615

    0 => 278
    1 => 116
    2 => 967

    ghci> runGenFn (arbitrary :: Gen (Int -> Bool)) [0, 1, 2]
    0 => False
    1 => True
    2 => False

    0 => True
    1 => False
    2 => True

    0 => True
    1 => False
    2 => False

    0 => True
    1 => False
    2 => False

    0 => True
    1 => True
    2 => True

    0 => True
    1 => True
    2 => False

    0 => False
    1 => True
    2 => False

    0 => True
    1 => True
    2 => False

    0 => True
    1 => True
    2 => True

    ghci> runGenFn (arbitrary :: Gen ([Int] -> [Int])) [[42], [0, 1, 2]]
    [42] => [-93,-540,-715,-557,-249]
    [0,1,2] => [433,97,665,554,-690,635]

    [42] => [-785,174,-676,-662,-549]
    [0,1,2] => [-735,-328,226,-524,423]

    [42] => [157,976,-774]
    [0,1,2] => [-197,608,-520,-37,-689]

    [42] => [-684]
    [0,1,2] => [902,-138,-33,689,-774,-713,474,-638]

    [42] => [-782,540,649,320,-326,92,896,-76]
    [0,1,2] => [156]

    [42] => [524,137]
    [0,1,2] => [642,-763,771,-400,825,71,895,586,252,37]

    [42] => [641,-304,-890,-375,449,-608,662,546,-740,-406]
    [0,1,2] => [-632,-685,-232,202,-994,666,-121]

    [42] => [200,599,-844]
    [0,1,2] => [-554,149,370,547,-755,-706,131]

    [42] => [-898,645,-472]
    [0,1,2] => [-77]

Appendix
--------

To get the code above to work we need instances for ``Monad``, ``Functor``, and ``Applicative``. These
are all lifted from [Test.QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck.html).

> instance Monad Gen where
>     return :: a -> Gen a
>     return a = Gen $ \_ -> a
>
>     (>>=) :: Gen a -> (a -> Gen b) -> Gen b
>
>     (Gen g) >>= f = Gen $ \r ->
>        let (r1, r2) = split r
>            Gen k = f $ g r1
>         in k r2
>
> instance Functor Gen where
>     fmap f (Gen h) = Gen $ \r -> f (h r)
> 
> instance Applicative Gen where
>     pure x = Gen $ \_ -> x
> 
>     f <*> x = do
>         f' <- f
>         x' <- x
>         return $ f' x'
>
> generate :: Gen a -> IO a
> generate (Gen g) = do
>     stdgen <- getStdGen
>     return $ g stdgen

Use ``sequence`` to get a few samples. Note that this relies on the ``Applicative``
instance's definition of ``<*>`` to get a new standard number generator each time it is used,
which in turn uses the ``Monad`` instance's definition which uses ``split``.

> samples :: Gen a -> IO [a]
> samples g = generate $ sequence [g, g, g, g, g, g, g, g, g]

> instance Arbitrary Int where
>   arbitrary = Gen $ \stdgen ->
>     let (r, _) = randomR (-1000, 1000) stdgen in r
>
> choose :: Random a => (a,a) -> Gen a
> choose range = Gen $ \stdgen ->
>   let (r, _) = randomR range stdgen in r
>
> vectorOf :: Int -> Gen a -> Gen [a]
> vectorOf = replicateM
>
> listOf :: Gen a -> Gen [a]
> listOf g = do
>   k <- choose (1, 10)
>   vectorOf k g
>
> instance Arbitrary a => Arbitrary [a] where
>   arbitrary = listOf arbitrary
