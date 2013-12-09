This is a literate Haskell file in "Bird" syntax. To compile to html use BlogLiterately.

<p> To install BlogLiterately: </p>

<pre>
cabal update
cabal install BlogLiterately
</pre>

<p> Make sure that <code>\$HOME/.cabal/bin</code> is in your
<code>$PATH</code>. Then to compile <code>Foo.lhs</code> to
<code>Foo.html</code> run: </p>

<pre>
cat header.html        >  Foo.html
BlogLiterately Foo.lhs >> Foo.html
</pre>

> module Foo where

> f :: Int -> Int
> f x = x + 1
