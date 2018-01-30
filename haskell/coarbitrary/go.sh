~/work/github/BlogLiterately/.cabal-sandbox/bin/BlogLiterately --rawlatex CoArbitrary.lhs | sed 's@<code class="sourceCode haskell">@&\n@'  | sed 's@<pre><code>@\n&\n@' | gvim -
