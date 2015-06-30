<p> Tinkering with lenses to deal with API changes. </p>

<p> FIXME github url. </p>

<p> First, some extensions and imports. </p>

> {-# LANGUAGE GADTs                        #-}
> {-# LANGUAGE FlexibleInstances            #-}
> {-# LANGUAGE MultiParamTypeClasses        #-}
> {-# LANGUAGE TemplateHaskell              #-}

> module LensHas where

> import Control.Applicative
> import Control.Lens
> import Numeric.Natural

<h2> Introduction </h2>

<p> Suppose we are working with a database service that stores files. Perhaps we communicate with it via a REST API. A file stored in the system
has a location, which is a <code>FilePath</code>: </p>

> type Location = FilePath

<p> We need to keep track of a few other things like the parent
(referring to a collection of files) and a hash of the file. For
simplicity I'll make those two fields <code>String</code>s since the details
aren't important to us here. </p>

> data DataFile = DataFile {
>     _dataFileLocation :: Location
>   , _dataFileParent   :: String
>   , _dataFileHash     :: String
> } deriving Show

<p> (Ignore the underscores if you haven't used lenses before.) </p>

<p> After some time the API changes and we need to keep track of some
different fields, so our data type changes to: </p>

> data DataFile2 = DataFile2 {
>     _dataFile2Location   :: Location
>   , _dataFile2Parent     :: String
>   , _dataFile2OtherField :: Float -- new field
>                                   -- hash is not here anymore
> } deriving Show

<p> For compatibility we'd like to keep both definitions around, perhaps allowing the user
to choose the v1 or v2 API with a configuration option. So how do we deal with our code that
has to use <code>DataFile</code> or <code>DataFile2</code>? One option is to use a sum type: </p>

< data DataFileSum = DFS1 DataFile | DFS2 DataFile2

<p> Any function that uses a <code>DataFile</code> must instead
use <code>DataFileSum</code> and do case analysis on whether it is
a v1 or v2. </p>

<p> In my particular situation I had a number of functions that used
just the <code>Location</code> part of the type. Is there a way to
avoid the sum type? </p>

<h2> Setter/Getter typeclasses </h2>

<p> Use typeclasses to represent setting or getting the location value: </p>

> class SetLocation a where
>   setLocation :: a -> Location -> a

> class GetLocation a where
>   getLocation :: a -> Location

<p> Write the instance definitions for each case: </p>

> instance SetLocation DataFile where
>   setLocation d newLocation = d { _dataFileLocation = newLocation }
>
> instance GetLocation DataFile where
>   getLocation = _dataFileLocation

> instance SetLocation DataFile2 where
>   setLocation d newLocation = d { _dataFile2Location = newLocation }
>
> instance GetLocation DataFile2 where
>   getLocation = _dataFile2Location

<p> Now we use the general <code>getLocation</code> and <code>setLocation</code> functions instead of
the specific data constructors of <code>DataFile</code> and <code>DataFile2</code>: </p>

> main1 = do
>   let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"
>
>   putStrLn $ "Original data file: " ++ show df
>   putStrLn $ "Location in original: " ++ getLocation df
>
>   let df' = setLocation df "/blah/bar.txt"
>
>   putStrLn $ "Updated data file:    " ++ getLocation df'

<p> A function that uses a datafile can now be agnostic about which one it is, as long as the typeclass
constraint is satisfied so that it has the appropriate getter/setter: </p>

> doSomething :: GetLocation a => a -> IO ()
> doSomething d = print $ getLocation d

<p> Using <code>doSomething</code>: </p>

<pre>
*LensHas> doSomething $ DataFile "/foo/bar.txt" "parent" "12345"
"/foo/bar.txt"

*LensHas> doSomething $ DataFile2 "/foo/bar.txt" "parent" 42.2
"/foo/bar.txt"
</pre>

<h2> Lenses </h2>

<p> Lenses already deal with the concept of getters and setters, so let's try to replicate the previous
code in that framework. </p>

<p> First, make lenses for the two data types (this uses Template Haskell): </p>

> makeLenses ''DataFile
> makeLenses ''DataFile2

<p> Instead of type classes for setting and getting, make a single type class
that represents the fact that a thing <i>has</i> a location.

> class HasLocation a where
>     location :: Lens' a Location

<p> For the instance definitions we can use the lenses that were automatically made for us by the earlier <code>makeLenses</code> lines: </p>

> instance HasLocation DataFile where
>     location = dataFileLocation :: Lens' DataFile Location
>
> instance HasLocation DataFile2 where
>     location = dataFile2Location :: Lens' DataFile2 Location

<p> Here is <code>main1</code> rewritten to use the <code>location</code> lens: </p>

> main2 = do
>   let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"
>
>   putStrLn $ "Original data file: " ++ show df
>   putStrLn $ "Location in original: " ++ df^.location
>
>   let df' = df & location .~ "/blah/bar.txt"
>
>   putStrLn $ "Updated data file:    " ++ getLocation df'

<p> If you haven't used lenses before the operators like <code>^.</code> might look insane, but there is a pattern to them. Check out <a href="http://intolerable.me/lens-operators-intro/">http://intolerable.me/lens-operators-intro</a> for an excellent guide with examples. </p>

<p> One benefit of the lens approach is that we don't have to
manually write the setters and getters, as they come for free
from the lenses for the original two data types. Another benefit
is that lenses compose, so if the <code>Location</code> type was more than just a string,
we wouldn't have to manually deal with the composition of <code>getLocation</code> with <code>getSubPartOfLocation</code> and so on. </p>

<p> The <code>doSomething</code> function can be rewritten using the <code>HasLocation</code> typeclass: </p>

> doSomething' :: HasLocation a => a -> IO ()
> doSomething' d = print $ d^.location

<h2> Generalising HasLocation </h2>

<p> Let's generalise the <code>HasLocation</code> typeclass. Consider natural numbers (the <code>Natural</code> type). </p>

<p> First case: here's a typeclass to represent the fact
that a <code>Foo</code> can always be thought of as a <code>Natural</code>: <?p>

> class AsNatural1 a where
>     nat1 :: Lens' a Natural

> data Foo = Foo {
>   _fooName :: String
> , _fooNat  :: Natural
> } deriving Show
>
> makeLenses ''Foo

> instance AsNatural1 Foo where
>   nat1 = fooNat :: Lens' Foo Natural

<p> Second case: a natural is a natural by definition. </p>

> instance AsNatural1 Natural where
>   nat1 = id

<p> Third case: an <code>Integer</code> might be a <code>Natural</code>. The previous typeclasses used a <code>Lens'</code> but here
we need a <code>Prism'</code>: </p>

> class AsNatural2 a where
>     nat2 :: Prism' a Natural

> instance AsNatural2 Integer where
>   nat2 = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)

<p> We are doing much the same thing, and if we compare the two
typeclasses the difference is in the type of "optical" thing
being used (a lens or a prism): </p>

< class AsNatural1 a where
<     nat1 :: Lens' a Natural
<
< class AsNatural2 a where
<     nat2 :: Prism' a Natural

<p> It turns out that the type to use is <code>Optic'</code>: </p>

> class AsNatural p f s where
>   natural :: Optic' p f s Natural

<p> (We get the extra parameters <code>p</code> and <code>f</code> which seem to be unavoidable.) </p>

<p> Now we can do all of the previous definitions using the single typeclass: </p>

> -- Lens into Foo:
>
> instance (p ~ (->), Functor f) => AsNatural p f Foo where
>   natural = fooNat :: Lens' Foo Natural
>
> -- Natural is a Natural:
>
> instance AsNatural p f Natural where
>   natural = id
>
> -- An Integer might be a natural:
>
> instance (Choice p, Applicative f) => AsNatural p f Integer where
>   natural = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)

<p> Now we can work with a <code>Foo</code>, a <code>Natural</code>, or
an <code>Integer</code> as a <code>Natural</code> by using the single
optical <code>natural</code>: </p>

> main3 :: IO ()
> main3 = do
>   -- Underlying thing is a Lens:
>   print $ (Foo "name" 34) ^. natural
>   print $ (Foo "name" 34) ^. natural + 1
>   print $ (42 :: Natural) ^. natural + 1
>
>   -- Underlying thing is a Prism (hence the applicative form):
>   print $ (+1) <$> ((50 :: Integer)  ^? natural)
>   print $ (+1) <$> ((-99 :: Integer) ^? natural)

<p> Output: </p>

<pre>
*LensHas> main3
34
35
43
Just 51
Nothing
</pre>

<h2> Credit </h2>

<p> The <code>AsNatural</code> type is a simplified version of the "As..." typeclasses
in the <a href="http://hackage.haskell.org/package/coordinate">coordinate package</a>, e.g. <a href="http://hackage.haskell.org/package/coordinate-0.0.18/docs/Data-Geo-Coordinate-Minutes.html#t:AsMinutes">AsMinutes</a>. Thanks to Tony Morris on #haskell.au for
helping with my changing-API question and pointing out the "As..." typeclasses. Also see the IRC logs in <a href="https://github.com/NICTA/coordinate/tree/master/etc">coordinate/etc</a> where <a href="https://twitter.com/kmett">Ed Kmett</a> explains some things about Optic. </p>



