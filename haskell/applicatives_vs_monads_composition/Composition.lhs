Note to self

<h3> Introduction </h3>

<p> Make sense of this: "applicatives compose, monads do not." </p>

<p> More formally, the statement is:
let <code>f</code> and <code>g</code> be
<a href="http://www.haskell.org/haskellwiki/Applicative_functor">applicative functors</a>. Then the
composition <code>f g</code> is also an applicative functor. On the other hand, there exist
<a href="http://www.haskell.org/haskellwiki/Monad">monads</a>
<code>f</code> and <code>g</code> such that <code>f g</code> is not a monad.  </p>

<h3> Composing functors </h3>

<p> First we will show how to compose two functors. To make things
concrete, let's do some examples with two data types that are instances of the
<code>Functor</code> class in Haskell: <code>[]</code> (lists),
and <code>Maybe</code>. We can check this using ghci: </p>

<pre>
Prelude> :set prompt "ghci> "
ghci> :set +t
ghci> :m +Data.Maybe
ghci> :info Maybe
data Maybe a = Nothing | Just a     -- Defined in `Data.Maybe'
instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'
instance Monad Maybe -- Defined in `Data.Maybe'
instance Functor Maybe -- Defined in `Data.Maybe'
instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
instance Show a => Show (Maybe a) -- Defined in `GHC.Show'
ghci> :info []
data [] a = [] | a : [a]    -- Defined in `GHC.Types'
instance Eq a => Eq [a] -- Defined in `GHC.Classes'
instance Monad [] -- Defined in `GHC.Base'
instance Functor [] -- Defined in `GHC.Base'
instance Ord a => Ord [a] -- Defined in `GHC.Classes'
instance Read a => Read [a] -- Defined in `GHC.Read'
instance Show a => Show [a] -- Defined in `GHC.Show'
</pre>

<p> In particular, note these lines: </p>

<pre>
instance Functor Maybe -- Defined in `Data.Maybe'
...
instance Functor [] -- Defined in `GHC.Base'
</pre>

<p> Composing <code>[]</code> with <code>Maybe</code> means that we have a list of
<code>Maybe</code> values, for example: </p>

<pre>
ghci> [Just 1, Nothing, Just 42] :: [] (Maybe Int)
[Just 1,Nothing,Just 42]
</pre>

<p> Our goal is to write a <code>Functor</code> instance declaration
for the general form of this composition, which means having a data
type that represents the composition itself. Often it's easier to
start from a specific example and work up to the general case. So
let's start with a list of <code>Maybe</code> values: </p>

> {-# LANGUAGE FlexibleInstances, InstanceSigs #-}
> 
> module Compose01 where
>
> import Data.Maybe()
>
> data Compose1 = MkCompose1 ([] (Maybe Int))

<p> where I have prefixed the data constructor with "Mk" to
disambiguate it from the data <i>type</i> which is <code>Compose</code>
(this can be helpful for newcomers to Haskell who are not familiar with
the usual practice of giving the data type and data constructors the
same name). Now generalise on the inner-most type, <code>Int</code>,
by making it a parameter: </p>

> data Compose2 x = MkCompose2 ([] (Maybe x))

<p> Next, generalise on the inner-most data constructor, <code>Just</code>, by making it a parameter: </p>

> data Compose3 g x = MkCompose3 ([] (g x))

<p> Finally, generalise on the list constructor <code>[]</code>, making it a parameter: </p>

> data Compose f g x = MkCompose (f (g x))

<p> and this is our definition of <code>Compose</code>. It lets us
represent any composition of data constructors. We can play around with it in ghci: </p>

<pre>
Compose01> :t MkCompose 
MkCompose :: f (g x) -> Compose f g x

Compose01> :t MkCompose [[42]] -- list of list
MkCompose [[42]] -- list of list :: Num x => Compose [] [] x

Compose01> :t MkCompose [Just 3, Just 42, Nothing] -- list of Maybe
MkCompose [Just 3, Just 42, Nothing] :: Num x => Compose [] Maybe x
</pre>

<p> Next, we have to fill in the definition of <code>fmap</code>
in an instance declaration for <code>Functor</code>: </p>

< instance (Functor f, Functor g) => Functor (Compose f g) where
<     fmap f (Compose x) = ...

<p> Again, use a concrete example and ghci to guide us. The inner-most
type is a <code>Maybe</code>, and being an instance of <code>Functor</code> means that 
we can use<code>fmap</code> to apply a
function to a "boxed" value: </p>

<pre>
Compose01> fmap (\x -> x + 1) (Just 42)
Just 43

Compose01> fmap (\x -> x + 1) (Nothing)
Nothing

Compose01> :t fmap (\x -> x + 1)
fmap (\x -> x + 1) :: (Functor f, Num b) => f b -> f b
</pre>

<p> So this function, <code>fmap (\\x -> x + 1)</code>, can be applied
to a list using <code>fmap</code> again: </p>

<pre>
Compose01> fmap (fmap (\x -> x + 1)) [Just 3, Just 42, Nothing] :: [] (Maybe Int)
[Just 4,Just 43,Nothing]
</pre>

<p> Generalise this by replacing the function 
<code>(\\x -> x + 1)</code> with <code>f</code> and the value
<code>[Just 3, Just 42, Nothing]</code> with the value <code>z</code>, and we get what turns out to be
the correct definition for the instance declaration: </p>

> instance (Functor f, Functor g) => Functor (Compose f g) where
>     fmap f (MkCompose x) = MkCompose (fmap (fmap f) x)

<p> An exercise for the reader is to check that with this definition
of <code>fmap</code>, the functor laws hold: </p>

< fmap id = id
< fmap (p . q) = (fmap p) . (fmap q)

<p> Now that <code>Compose</code> is an instance of <code>Functor</code>, we can use a single <code>fmap</code> to apply a function on values that are wrapped up in <code>Compose</code>:

<pre>
Compose01> fmap (\x -> x + 1) (MkCompose [Just 3, Just 42, Nothing])
MkCompose [Just 4,Just 43,Nothing]
</pre>

<h3>Applicatives compose </h3>

<p> To show that applicatives compose, we need to write the instance declaration for <code>Applicative</code>: </p>

< instance (Applicative f, Applicative g) => Applicative (Compose f g) where
<     pure x  = ...
<     f <*> x = ...

<p> This one is a bit more complicated than the <code>Functor</code>
instance so I made a short screencast on how to use
<a href="http://matthew.brecknell.net/post/hole-driven-haskell/">hole-driven development</a>
to find the answer. With hole-driven development we have a bit of
a conversation with the type system and this is easier to show in a
narrated screencast compared to a linear written text. </p>

<p> <center>
<iframe width="560" height="315" src="//www.youtube.com/embed/AjtQ0sQaHn0?vq=hd1080" frameborder="0" allowfullscreen></iframe>
</center>
</p>

<p> If you don't want to watch the screencast, just take my word that we can fill in the definition for
the <code>Compose</code> instance of <code>Applicative</code>. (Or, sneak a peek at the source code
for <a href="http://hackage.haskell.org/package/applicative-extras-0.1.8/docs/Control-Applicative-Compose.html">Control.Applicative.Compose</a>.)
Another exercise for the reader: verify that the following
<a href="http://en.wikibooks.org/wiki/Haskell/Applicative_Functors">functor laws</a> hold. </p>

< pure id <*> v = v                            -- Identity
< pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
< pure f <*> pure x = pure (f x)               -- Homomorphism
< u <*> pure y = pure ($ y) <*> u              -- Interchange
< fmap f x = pure f <*> x                      -- Fmap (on the Functor instance)

<h3> Monads do not compose </h3>

<p> To show that "monads do not compose", it is sufficient to find a counterexample, namely two
<a href="http://www.haskell.org/haskellwiki/Monad">monads</a>
<code>f</code> and <code>g</code> such that <code>f g</code> is not a monad. In particular,
we will show that one of the <a href="http://www.haskell.org/haskellwiki/Monad_laws">monad laws</a>
is violated for <i>any</i> possible instance declaration. </p> 

<p> The following is just an expanded version of
<a href="http://stackoverflow.com/a/13209294">Conor McBride's answer on stackoverflow</a>
so all credit goes to him, and any mistakes here are my
responsibility. Conor's proof is the shortest and easiest to explain
counterexample that I could find.</p>

<p> First, define the terminal monad <code>Thud</code>: </p>

> data Thud a = MkThud deriving (Show)

<p> Note that it has an unused type parameter. We have to do this so that the
<a href="http://www.haskell.org/haskellwiki/Kind">kind</a> is correct for the <code>Monad</code> instance.
The instance declaration for <code>Monad</code> is quite easy because
we only have a single way of creating a <code>Thud</code> value: </p>

> instance Monad Thud where
>     return _ = MkThud
>     _ >>= _  = MkThud

<p> Playing around with ghci, we see that anything turns into a <code>Thud</code>: </p>

<pre>
ghci> return 0 :: Thud Int
MkThud

ghci> (return 0 :: Thud Int) >>= (\x -> return (x + 1))
MkThud
</pre>


<p> The other data type is <code>Flip</code>, which wraps a value along with a boolean: </p>

> data Flip a = MkFlip Bool a deriving (Show)

<p> The <code>Monad</code> instance is of a
<a href="http://www.haskell.org/haskellwiki/All_About_Monads#The_Writer_monad">writer monad</a> with an
xor structure: </p>

> instance Monad Flip where
>     return :: a -> Flip a
>     return = MkFlip False   -- or, return x = MkFlip False x
> 
>     (>>=) :: Flip a -> (a -> Flip b) -> Flip b
>     MkFlip False x >>= f = f x
>     MkFlip True  x >>= f = MkFlip (not b) y
>         where MkFlip b y = f x

<p> Informally, <code>return</code> wraps a value along with the <code>False</code> value. The bind (<code>>>=</code>) function will
apply the monadic function <code>f</code> if we have a <code>False</code> value, otherwise it will apply <code>f</code> but flip its boolean component for
the final result. </p>

<p> Some example values and computations: </p>

<pre>
ghci> (return "boo" :: Flip String)
MkFlip False "boo"

ghci> (return "boo" :: Flip String) >>= (\x -> return $ x ++ " hey!")
MkFlip False "boo hey!"

ghci> (return "boo" :: Flip String) >>= (\x -> return $ x ++ " hey!") >>= (\x -> return $ x ++ " Huh?")
MkFlip False "boo hey! Huh?"

ghci> (return "boo" :: Flip String) >>= (\x -> MkFlip True (x ++ " hey!"))
MkFlip True "boo hey!"

ghci> (return "boo" :: Flip String) >>= (\x -> MkFlip True (x ++ " hey!")) >>= (\x -> return $ x ++ " What?")
MkFlip True "boo hey! What?"

ghci> (return "boo" :: Flip String) >>= (\x -> MkFlip True (x ++ " hey!")) >>= (\x -> MkFlip True (x ++ " What?"))
MkFlip False "boo hey! What?"
</pre>

<p> Finally we come to the <code>Monad</code> instance for <code>Compose</code> for the specific case of a <code>Flip</code> of <code>Thud</code>: </p>

< instance Monad (Compose Flip Thud) where
<     return x = undefined
<     x >>= f  = undefined

<p> Let's start with <code>return</code>. It has to produce something of
type <code>Compose Flip Thud a</code>, so we begin with the type constructor: </p>

<     return x = MkCompose (MkFlip ??? MkThud)

<p> This is all we can do - we are constrained by the types. Now
what can go in the place of the three question marks? Perhaps a function of <code>x</code>, say </p>

<     return x = MkCompose (MkFlip (h x) MkThud)

<p> where <code>h :: a -> Bool</code>. However, Haskell has the parametricity property. Quoting the
<a href="http://www.haskell.org/haskellwiki/Polymorphism#Parametric_polymorphism">Haskell wiki</a>: </p>

<blockquote>
<p> Since a parametrically polymorphic value does not "know" anything
about the unconstrained type variables, it must behave the same
regardless of its type. This is a somewhat limiting but extremely
useful property known as parametricity.  </p> </blockquote>

<p> So parametricity implies that the function <code>h</code> can't be something like </p>

<pre>
h x = if (x is of type blah)
        then True
        else ...                      
</pre>

<p> which means that <code>h</code> must be a constant, and therefore <code>return</code> is also a constant.
Without loss of generality, suppose that the definition is</p>

< instance Monad (Compose Flip Thud) where
<     return :: a -> Compose Flip Thud a
<     return x = MkCompose (MkFlip True MkThud)

<p> The left identity monad law says that </p>

< return x >>= f = f x

<p> for any appropriately typed <code>f</code> and <code>x</code>. Since
<code>return</code> is a constant, we have </p>

< (MkCompose (MkFlip True MkThud)) >>= f = f x

<p> Let <code>f = id</code>, then we have two equations using the two
values that exist of type <code>Compose Flip Thud</code>: </p>

< (MkCompose (MkFlip True MkThud)) >>= id = id (MkCompose (MkFlip True  MkThud))
< (MkCompose (MkFlip True MkThud)) >>= id = id (MkCompose (MkFlip False MkThud))

<p> which implies that </p>

< id (MkCompose (MkFlip True MkThud)) = id (MkCompose (MkFlip False MkThud))

<p> which is a contradiction. So it is not possible to define <code>return</code> and <code>>>=</code> in a consistent manner
for the <code>Compose Flip Thud</code> instance of the <code>Monad</code> typeclass. We conclude that in general it is not true that
the composition <code>f g</code> will be a monad for any two monads <code>f</code> and <code>g</code>. </p>

<h3> Further reading </h3>

<ul>



<li> Stack Overflow question on composition of applicatives vs monads: <a href="http://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont">http://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont</a></li>

<li> Another Stack Overflow question: <a href="http://stackoverflow.com/questions/13034229/concrete-example-showing-that-monads-are-not-closed-under-composition-with-proo">http://stackoverflow.com/questions/13034229/concrete-example-showing-that-monads-are-not-closed-under-composition-with-proo</a></li>

<li> Philip Wadler's papers on parametricity: <a href="http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html">http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html</a></li>

<li> A paper about conditions under which monads <i>do</i> compose: <a href="http://arxiv.org/pdf/0710.1120v1.pdf">Eugenia Cheng: Iterated distributive laws</a>. See also the first Stack Overflow <a href="http://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont">link</a> for 
comments about a <code>swap</code> function for reversing the nesting of the monads. </li>

 </ul>


