> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE InstanceSigs #-}
>
> module Notes where
>
> import qualified Data.Text as T
> import qualified Data.Map as M
>
> -- import Control.Monad.Free
> import Control.Monad (forM_)
>
> import Safe (lastDef)
>
> import Data.Maybe (fromJust)
>
> -- My usual friends.
> data Hole = Hole
> hole :: forall a. a
> hole = undefined

<p> Motivating example: model a data store. This could be an in-memory
dictionary, an sqlite file on disk, a postgresql table, etc. </p>

<p> Parameterise the type by the type of the index, <code>i</code>,
and the type of the values, <code>v</code>. </p>

< data DataStore i v = Create   (v  -> i)
<                    | List     (() -> [v])
<                    | Retrieve (i  -> v)
<                    | Update v
<                    | Delete i

<p> How to read the types: for <code>Create</code>, <code>List</code>,
and <code>Retrieve</code>, an action is performed. So it's natural
that these three constructors will store a function type. </p>

<ul>
<li> <code>Create</code>: we supply a value <code>v</code>, and get back an index <code>i</code>. </li>
<li> <code>List</code>:   we supply nothing (I guess in a more general setting we
          could supply a filter?) and get back a list of values. </li>
<li> <code>Retrieve</code>: We supply an index and get back a value. </li>
</ul>

<p> On the other hand, <code>Update </code>and <code>Delete </code>are
"in-place". In theory we could return a list of affected indexes,
<code>Update </code>could have been <code>Update (v -> [i])
</code>. But we'll leave it as just <code>Update v</code>. Lastly,
<code>Delete </code>just deletes the index/value pair, so we don't
have anything to return, so it's not a function type. </p>

<p> To make use of <code>Free </code>we need our data store to be an instance of <code>Functor</code>.
Check the kind of <code>Free </code>in ghci: </p>

<pre>
ghci> :k Free
Free :: (* -> *) -> * -> *
</pre>

<p> However our <code>DataStore </code>has a concrete type. So add another parameter
<code>k </code>and tweak the types: </p>

> data DataStoreF i v k = Create v   (i   -> k)
>                       | List       ([v] -> k)
>                       | Retrieve i (v   -> k)
>                       | Update v k
>                       | Delete i k

<p> This is a bit like a continuation - reading the types: </p>

<p> <ul>
<li> <code>Create</code>: the value <code>v </code>is part of the constructor, and the last bit is a
          function that uses the new index <code>i </code>and produces some <code>k</code>. </li>
<li> <code>List</code>:   now the annoying <code>()</code>  has  gone, and the constructor just holds
          the function which takes the list <code>[v]</code> and does something with it,
          returning something of type <code>k</code>. </li>
<li> <code>Retrieve</code>: the index is stored along with the function that uses the
            retrieved value, returning something of type <code>k</code>. </li>
<li> <code>Update</code>: As before, but since nothing is done "to" the updated
          value, we can just store the value of type <code>k</code>.
<li> <code>Delete</code>: similar to Update. </li>
</ul> </p>

<p> For the Functor instance we can use the compiler extension
DerivingInstances or we can grind through the details ourselves. It's
easy using hole-driven development because there are so few choices
for each definition. </p>

> instance Functor (DataStoreF i v) where
>     fmap f (Create v k)   = Create v   (f . k)
>     fmap f (List k)       = List       (f . k)
>     fmap f (Retrieve i k) = Retrieve i (f . k)
>     fmap f (Update v k)   = Update v (f k)
>     fmap f (Delete i k)   = Delete i (f k)

<p> We can sequence some commands manually. </p>

<p> Create 3, then delete that same value, and finally return <code>()</code>. </p>

> manual1 :: DataStoreF i Integer (DataStoreF i v ())
> manual1 = Create 3 (\i -> (Delete i ()))

<p> Create 3, then create 4, then create 5, and return the triple
consisting of the index values for 3, 4, and 5, respectively: </p>

> manual2 :: DataStoreF t Integer (DataStoreF t Integer (DataStoreF t Integer (t, t, t)))
> manual2 = Create 3 (\i -> (Create 4 (\j -> Create 5 (\k -> (i, j, k)))))

<p> Even though <code>manual1</code> and <code>manual2</code> are sequences
of commands, their type changes (and gets longer as more commands are
added). But we can use <code>Fix</code>(or <code>Free</code>), which can be thought of as the <a href="http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html">fixed point of a functor</a>.  </p>

<p> Instead of using <code>Control.Monad.Free</code> (as in dalaing's BFPG talk),
we'll write our own definition of <code>Free</code>: </p>

> data Free f r = MkFree (f (Free f r)) | MkPure r

<p> Now a data store with index <code>i</code>, key <code>v</code>, and return type <code>r</code>: </p>

> type DataStore i v r = Free (DataStoreF i v) r

<p> Now <code>manual1</code>and <code>manual2</code> have a succint type: </p>

> manual1' :: DataStore i Integer ()
> manual1' = MkFree $ Create 3 (\i ->
>                     (MkFree $ Delete i (MkPure ())))

> manual2' :: Free (DataStoreF i Integer) (i, i, i)
> manual2' = MkFree $ Create 3 (\i ->
>                     MkFree $ Create 4 (\j ->
>                     MkFree $ Create 5 (\k ->
>                     MkPure (i, j, k))))

<p> This looks suspiciously like the monad bind syntax, desugared In fact,
for any functor <code>f</code>, we can write the instance for <code>Monad (Free f)</code>: </p>

> instance (Functor f) => Monad (Free f) where
>     return :: a -> Free f a
>     return x = MkPure x
>
>     (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>     (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
>     (MkPure r) >>= f = f r

<p> The function return puts a value into the monad, which uses
<code>MkPure</code>. The bind definition is a bit more complicated,
but it can be worked out using hole driven development: </p>

<p> The two parameters: </p>

< x :: f (Free f a)
< h :: a -> Free f b

<p> Use the <code>MkFree</code> constructor, poke the hole: </p>

<     (MkFree x) >>= h = MkFree Hole

<pre>
Couldn't match expected type `f (Free f b)' with actual type `Hole'
</pre>

<p> We have <code>h</code> which produces a <code>Free f b</code> from an <code>a</code> but <code>x</code> has type <code>f (...)</code>. So maybe <code>fmap</code> will help: </p>

<     (MkFree x) >>= h = MkFree $ fmap Hole x

<p> Now it says: </p>

<pre>
Couldn't match expected type `Free f a -> Free f b'
</pre>

<p> Make it a function: </p>

<     (MkFree x) >>= h = MkFree $ fmap (\q -> Hole) x

<p> Which now complains: </p>

<pre>
Couldn't match expected type `Free f b' with actual type `Hole'
</pre>

<p> Temporarily turn <code>Hole</code> into <code>hole</code> and use
ghc-mod to check the type of <code>q</code>. We now have these terms: </p>

< q :: Free f a
< h :: a -> Free f b

<p> But look at the type of <code>>>=</code>, it is exactly what we need now: </p>

< (>>=) :: Free f a -> (a -> Free f b) -> Free f b

<p> So plug it in and it type checks. (We have to manually verify the monad laws ourselves.) </p>

<     (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x

<p> This can also be written as </p>

<     (MkFree x) >>= h = MkFree $ fmap (>>= h) x

<p> Getting better: </p>

> manual1'' :: DataStore i Integer ()
> manual1'' = do
>   i <- MkFree $ Create 3 (\x0 -> MkPure x0)
>   MkFree $ Delete i (MkPure ())
>   return ()

> manual2'' :: DataStore i Integer (i, i, i)
> manual2'' = do
>   i <- MkFree $ Create 3 (\x0 -> MkPure x0)
>   j <- MkFree $ Create 4 (\x1 -> MkPure x1)
>   k <- MkFree $ Create 5 (\x2 -> MkPure x2)
>   return (i, j, k)

<p> Some helper functions: </p>

> create :: v -> DataStore i v i
> create x = MkFree $ Create x (\i -> MkPure i)

> delete :: i -> DataStore i v ()
> delete i = MkFree $ Delete i (MkPure ())

> manual1''' :: DataStore i Integer ()
> manual1''' = do
>   i <- create 3
>   delete i
>   return ()

> manual2''' :: DataStore i Integer (i, i, i)
> manual2''' = do
>   i <- create 3
>   j <- create 4
>   k <- create 5
>   return (i, j, k)


<p> So we have a nifty little DSL! </p>

<p> We can write an "interpreter" that converts a series of data store
commands into a string: </p>

> printDataStore :: (Show i, Show r) => DataStore i i r -> String

> printDataStore (MkFree (Create v next)) = "Create: " ++ show v ++ "\n" ++ printDataStore (next v)

<p> Notice how the 'next' thing is a function, so to print it we first apply it to the index value that we have. </p>

< v    :: i
< next :: i -> Free (DataStoreF i i) r

<p> With <code>List</code> we run into a problem - because we aren't
actually computing the data store, we have no way of producing an
actual list to print here. So we just use the empty list as a dummy
list for the parameter to 'next'. </p>

> printDataStore (MkFree (List next)) = "List (dummy list): [pretend list] " ++ "\n" ++ printDataStore (next dummyList)
>   where dummyList = [] :: [i]

<p> The rest of these are straightforward: </p>

> printDataStore (MkFree (Retrieve i next)) = "Retrieve: " ++ show i ++ "\n" ++ printDataStore (next i)
> printDataStore (MkFree (Delete i next)) = "Delete: " ++ show i ++ "\n" ++ printDataStore next
> printDataStore (MkFree (Update i next)) = "Update: " ++ show i ++ "\n" ++ printDataStore next
> printDataStore (MkPure x) = "Pure value: " ++ show x

<p> Some examples: </p>

<pre>
ghci> putStrLn $ printDataStore manual1'''
Create: 3
Delete: 3
Pure value: ()

ghci> putStrLn $ printDataStore manual2'''
Create: 3
Create: 4
Create: 5
Pure value: (3,4,5)
</pre>

<p> Make a few more helpers so we can do a longer example: </p>

> list :: DataStore i v [v]
> list = MkFree $ List (\x -> MkPure x)

> retrieve :: i -> DataStore i v v
> retrieve i = MkFree $ Retrieve i (\x -> MkPure x)

> update :: v -> DataStore i v ()
> update v = MkFree $ Update v (MkPure ())

> longExample :: DataStore Integer Integer (Integer, [Integer])
> longExample = do
>   i <- create 3
>   j <- create 4
>   values <- list
>   v <- retrieve i
>   return (v, values)

<p> So while the value of the index for '3' passes through as 'i', the
list stored in 'values' is just the empty list as can be seen in the
final value: </p>

<pre>
ghci> putStrLn $ printDataStore longExample
Create: 3
Create: 4
List (dummy list): [pretend list]
Retrieve: 3
Pure value: (3,[])
</pre>

<p> It's a bit like having a "null" back end that ignores all commands
to store data and always returns an empty list. </p>

