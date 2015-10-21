<p> Note to self about deriving the <code>Profunctor</code> typeclass. </p>

<p> This is a literate Haskell file, and it can be built
using <a href="https://github.com/commercialhaskell/stack">Stack</a>: </p>

<pre>
git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/profunctors
stack build
</pre>

<p> Then use <code>stack ghci</code> instead of <code>cabal repl</code>. The main executable is
in a path like <code>./.stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/profunctors-exe</code>.  </p>

<p> This blog post follows some of the examples from <a href="https://www.fpcomplete.com/user/liyang/profunctors">I love profunctors.</a> </p>

<p> First, some extensions and imports: </p>

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE InstanceSigs          #-}
> {-# LANGUAGE RankNTypes            #-}
> {-# LANGUAGE ScopedTypeVariables   #-}
>
> module Profunctors where
>
> import Control.Applicative
> import Data.Char
> import Data.Functor.Constant
> import Data.Functor.Identity
> import Data.Tuple (swap)
> import qualified Data.Map as M
>
> main = print "boo"

<h2> Motivating example </h2>

<p> The basic problem here is to write a function that
capitalizes each word in a string. First, write a
function that capitalizes a single word: </p>

> capWord :: String -> String
> capWord [] = []
> capWord (h:t) = (toUpper h):(map toLower t)

<p> The straightforward solution (ignoring the loss of extra spaces
between words since <code>unwords . words</code> is not an isomorphism)
is to use this composition: </p>

> capitalize :: String -> String
> capitalize = unwords . (map capWord) . words

<p> Example output: </p>

<pre>
*Profunctors> capitalize "hey yo WHAT DID THIS          DO?"
"Hey Yo What Did This Do?"
</pre>

<p> Why stop here? Let's generalise the <code>capitalize</code>
function by factoring out the <code>words</code>
and <code>unwords</code> functions. Call them <code>w</code>
and <code>u</code> and make them arguments: </p>

> capitalize1 :: (String -> [String]) -> ([String] -> String) -> String -> String
> capitalize1 w u = u . (map capWord) . w

<p> Now, <code>capitalize ≡ capitalize1 words unwords</code>. </p>

<p> We may as well factor out <code>map capWord</code> as well: </p>

> capitalize2 :: (String -> [String])
>              -> ([String] -> String)
>              -> ([String] -> [String])
>              -> String -> String
> capitalize2 w u f = u . f . w

<p> We have: <code>capitalize ≡ capitalize2 words unwords (map capWord)</code>. </p>

<p> Now look at the types - there is no reason to be restricted
to <code>String</code> and <code>[String]</code> so use the most
general types that make the composition <code>u . f . w</code> work: </p>

<pre>
     w          f          u
c -------> d -------> b -------> d
</pre>

<p> so <code>w :: c -> d</code> and similar for <code>f</code> and <code>u</code>.
This lets us write </p>

> capitalize3 :: (c -> a)
>             -> (b -> d)
>             -> (a -> b)
>             -> (c -> d)
> capitalize3 w u f = u . f . w

<p> Next, we can generalize the type of <code>f</code>. To help with this step, recall
that <code>-></code> is a functor (there is an instance <code>Functor (->)</code>) so write
the last two types in the signature with prefix notation: </p>

> capitalize3' :: (c -> a)
>              -> (b -> d)
>              -> (->) a b
>              -> (->) c d
> capitalize3' w u f = u . f . w

<p> Now we can use a general functor <code>h</code> instead of <code>-></code>: </p>

< capitalize4 :: (c -> a)
<             -> (b -> d)
<             -> h a b -- was (->) a b
<             -> h c d -- was (->) c d
< capitalize4 w u f = u . f . w

<p> Naturally this won't work because the type signature has the
functor <code>h</code> but the body of <code>capitalize4</code>
is using function composition (the <code>.</code>) as the type error
shows: </p>

<pre>
 | Couldn't match type ‘h’ with ‘(->)’
||   ‘h’ is a rigid type variable bound by
||       the type signature for
||         capitalize3' :: (c -> a) -> (b -> d) -> h a b -> h c d
|| Expected type: h c d
||   Actual type: c -> d
</pre>

<p> Fortunately for us, we can make a typeclass that captures the behaviour that we want.
We have actually arrived at the definition of a profunctor. </p>

> class Profunctor f where
>   dimap :: (c -> a) -> (b -> d) -> f a b -> f c d
>
> instance Profunctor (->) where
>   dimap :: (c -> a) -> (b -> d) -> (a -> b) -> c -> d
>   dimap h g k = g . k . h

<p> Now we can write the capitalize function using
a typeclass constraint on <code>Profunctor</code> which lets us
use the <code>dimap</code> function instead
of explicit function composition: </p>

> capitalize5 :: String -> String
> capitalize5 s = dimap words unwords (map capWord) s

<p> This is overkill for the capitalization problem, but it shows how structure
can come out of simple problems if you keep hacking away. </p>
