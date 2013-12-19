# Notes on tinkering with ghcmod-vim to get jump-to-definition for any symbol.

## Initial idea

This works for both the system-installed docs on Debian and
my custom build:

    $ ghc-mod info foo.hs Foo Just
    data Maybe a = ... | Just a 	-- Defined in `Data.Maybe'

    $ ghc-pkg find-module Data.Maybe --simple-output
    haskell2010-1.1.1.0 base-4.6.0.1

    $ ghc-pkg field base-4.6.0.1 haddock-html
    haddock-html: /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1

Does not ive the right answer for String, as it is defined in `GHC.Base`
which is internal, and instead comes from the `Prelude`:


    $ ghc-mod info foo.hs Foo String
    type String = [Char] 	-- Defined in `GHC.Base'

    $ ghc-pkg find-module GHC.Base
    /home/carlo/opt/ghc-7.6.3_build/lib/ghc-7.6.3/package.conf.d
       base-4.6.0.1
    /home/carlo/.ghc/x86_64-linux-7.6.3/package.conf.d

    $ ghc-pkg field base-4.6.0.1 haddock-html
    haddock-html: /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1

    $ ls /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/GHC-Base.html
    ls: cannot access /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/GHC-Base.html: No such file or directory

    $ ls /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/src/GHC-Base.html
    /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/src/GHC-Base.html

## Handy links

https://github.com/kazu-yamamoto/ghc-mod/pull/156

http://web.archiveorange.com/archive/v/2B0uXSV58foq8lPQPSZA

http://hub.darcs.net/simon/ghc-old/raw/compiler/main/InteractiveEval.hs

    -- | Parses a string as an identifier, and returns the list of 'Name's that
    -- the identifier can refer to in the current interactive context.
    parseName :: GhcMonad m => String -> m [Name]
    parseName str = withSession $ \hsc_env -> do
       (L _ rdr_name) <- liftIO $ hscParseIdentifier hsc_env str
       liftIO $ hscTcRnLookupRdrName hsc_env rdr_name


https://ghc.haskell.org/trac/ghc/ticket/1463


2013-12-14

    ci ghc-paths
    ghc-pkg expose ghc-7.6.3


did A.hs and B.hs from wiki page with some notes: http://www.haskell.org/haskellwiki/GHC/As_a_library

Similar problem namesInScope empty:

http://osdir.com/ml/cvs-ghc@haskell.org/2011-01/msg00576.html



http://www.cse.unsw.edu.au/~pls/repos/yi/Yi/Boot.hs


http://hackage.haskell.org/package/hint-0.2.1/docs/src/Language-Haskell-Interpreter-GHC.html#typeOf


https://github.com/faylang/fay/issues/269


Haskell-names can work out what a module exports:
    http://documentup.com/haskell-suite/haskell-names
    http://haskell-suite.github.io/docs/haskell-names/

So maybe we have to use our A.hs, work through the "textual imports" to see which one
spits out the thing we are using?


Useful looking blog post: http://parenz.wordpress.com/2013/08/17/ghc-api-interpreted-compiled-and-package-modules/

http://markmail.org/message/tqv5nruic74sbkde#query:+page:1+mid:6upbo6j34mr4s6jz+state:results





http://web.mit.edu/~ezyang/Public/ghc-cmm-09Dec10/compiler/rename/RnEnv.lhs



pprNameProvenance :: GlobalRdrElt -> SDocSource

Print out the place where the name was imported







http://osdir.com/ml/glasgow-haskell-users@haskell.org/2013-07/msg00028.html


