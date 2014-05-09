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

Motivating example: model a data store. This could be an in-memory
dictionary, an sqlite file on disk, a postgresql table, etc.

Parameterise the type by the type of the index, i, and the type of
the values, v.

< data DataStore i v = Create   (v  -> i)
<                    | List     (() -> [v])
<                    | Retrieve (i  -> v)
<                    | Update v
<                    | Delete i

How to read the types: for Create, List, and Retrieve, an action is
performed. So it's natural that these three constructors will store
a function type.

* Create: we supply a value v, and get back an index i.
* List:   we supply nothing (I guess in a more general setting we
          could supply a filter?) and get back a list of values.
* Retrieve: We supply an index and get back a value.

On the other hand, Update and Delete are "in-place". In theory we
could return a list of affected indexes, Update could have been Update
(v -> [i]). But we'll leave it as just Update v. Lastly, Delete just
deletes the index/value pair, so we don't have anything to return,
so it's not a function type.

To make use of Free we need our data store to be an instance of Functor.
Check the kind of Free in ghci:

    ghci> :k Free
    Free :: (* -> *) -> * -> *

However our DataStore has a concrete type. So add another parameter
k and tweak the types:

> data DataStoreF i v k = Create v   (i   -> k)
>                       | List       ([v] -> k)
>                       | Retrieve i (v   -> k)
>                       | Update v k
>                       | Delete i k

This is a bit like a continuation - reading the types:

* Create: the value v is part of the constructor, and the last bit is a
          function that uses the new index i and produces some k.
* List:   now the annoying () has gone, and the constructor just holds
          the function which takes the list [v] and does something with it,
          returning something of type k.
* Retrieve: the index is stored along with the function that uses the
            retrieved value, returning something of type k.
* Update: As before, but since nothing is done "to" the updated
          value, we can just store the value of type k.
* Delete: similar to Update.

For the Functor instance we can use the compiler extension
DerivingInstances or we can grind through the details ourselves. It's
easy using hole-driven development because there are so few choices
for each definition.

> instance Functor (DataStoreF i v) where
>     fmap f (Create v k)   = Create v   (f . k)
>     fmap f (List k)       = List       (f . k)
>     fmap f (Retrieve i k) = Retrieve i (f . k)
>     fmap f (Update v k)   = Update v (f k)
>     fmap f (Delete i k)   = Delete i (f k)

We can sequence some commands manually.

Create 3, then delete that same value, and finally return ().

> manual1 :: DataStoreF i Integer (DataStoreF i v ())
> manual1 = Create 3 (\i -> (Delete i ()))

Create 3, then create 4, then create 5, and return the triple
consisting of the index values for 3, 4, and 5, respectively:

> manual2 :: DataStoreF t Integer (DataStoreF t Integer (DataStoreF t Integer (t, t, t)))
> manual2 = Create 3 (\i -> (Create 4 (\j -> Create 5 (\k -> (i, j, k)))))

Even though manual1 and manual2 are sequences of commands, their type changes (and gets longer
as more commands are added). But we can use Fix (or Free), which can be thought of as
the "fixed point of a functor" [1].

Instead of using Control.Monad.Free (as in dalaing's BFPG talk),
we'll write our own definition of Free:

> data Free f r = MkFree (f (Free f r)) | MkPure r

Now a data store with index i, key v, and return type r:

> type DataStore i v r = Free (DataStoreF i v) r

Now manual1 and manual2 have a succint type:

> manual1' :: DataStore i Integer ()
> manual1' = MkFree $ Create 3 (\i ->
>                     (MkFree $ Delete i (MkPure ())))

> manual2' :: Free (DataStoreF i Integer) (i, i, i)
> manual2' = MkFree $ Create 3 (\i ->
>                     MkFree $ Create 4 (\j ->
>                     MkFree $ Create 5 (\k ->
>                     MkPure (i, j, k))))

This looks suspiciously like the monad bind syntax, desugared In fact,
for any functor f, we can write the instance for Monad (Free f):

> instance (Functor f) => Monad (Free f) where
>     return :: a -> Free f a
>     return x = MkPure x
>
>     (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>     (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
>     (MkPure r) >>= f = f r

The function return puts a value into the monad, which uses MkPure. The
bind definition is a bit more complicated, but it can be worked out
using hole driven development:

The two parameters:

< x :: f (Free f a)
< h :: a -> Free f b

Use the MkFree constructor, poke the hole:

<     (MkFree x) >>= h = MkFree Hole

    Couldn't match expected type `f (Free f b)' with actual type `Hole'

We have h which produces a Free f b from an a but x has type f
(...). So maybe fmap will help:

<     (MkFree x) >>= h = MkFree $ fmap Hole x

Now it says:

    Couldn't match expected type `Free f a -> Free f b'

Make it a function:

<     (MkFree x) >>= h = MkFree $ fmap (\q -> Hole) x

Which now complains:

    Couldn't match expected type `Free f b' with actual type `Hole'

Temporarily turn Hole into hole and use ghc-mod to check the type of
q. We now have these terms:

< q :: Free f a
< h :: a -> Free f b

But look at the type of >>=, it is exactly what we need now:

< (>>=) :: Free f a -> (a -> Free f b) -> Free f b

So plug it in and it type checks. (We have to manually verify the monad laws ourselves.)

<     (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x

This can also be written as

<     (MkFree x) >>= h = MkFree $ fmap (>>= h) x

Getting better:

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

Some helper functions:

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


So we have a nifty little DSL!

We can write an "interpreter" that converts a series of data store
commands into a string:

> printDataStore :: (Show i, Show r) => DataStore i i r -> String

> printDataStore (MkFree (Create v next)) = "Create: " ++ show v ++ "\n" ++ printDataStore (next v)

Notice how the 'next' thing is a function, so to print it we first apply it to the index value that we have.

< v    :: i
< next :: i -> Free (DataStoreF i i) r

With List we run into a problem - because we aren't actually computing
the data store, we have no way of producing an actual list to print
here. So we just use the empty list as a dummy list for the parameter
to 'next'.

> printDataStore (MkFree (List next)) = "List (dummy list): [pretend list] " ++ "\n" ++ printDataStore (next dummyList)
>   where dummyList = [] :: [i]

The rest of these are straightforward:

> printDataStore (MkFree (Retrieve i next)) = "Retrieve: " ++ show i ++ "\n" ++ printDataStore (next i)
> printDataStore (MkFree (Delete i next)) = "Delete: " ++ show i ++ "\n" ++ printDataStore next
> printDataStore (MkFree (Update i next)) = "Update: " ++ show i ++ "\n" ++ printDataStore next
> printDataStore (MkPure x) = "Pure value: " ++ show x

Some examples:

    ghci> putStrLn $ printDataStore manual1'''
    Create: 3
    Delete: 3
    Pure value: ()

    ghci> putStrLn $ printDataStore manual2'''
    Create: 3
    Create: 4
    Create: 5
    Pure value: (3,4,5)

Make a few more helpers so we can do a longer example:

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

So while the value of the index for '3' passes through as 'i', the
list stored in 'values' is just the empty list as can be seen in the
final value:

    ghci> putStrLn $ printDataStore longExample
    Create: 3
    Create: 4
    List (dummy list): [pretend list]
    Retrieve: 3
    Pure value: (3,[])

It's a bit like having a "null" back end that ignores all commands
to store data and always returns an empty list.







References

[1] http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

























Temporary stuff:

    > blap3 :: Free (DataStoreF r Integer) r
    > blap3 = MkFree $ Create 3 (\i -> MkPure i)

    > blap4 :: Free (DataStoreF r Integer) r
    > blap4 = MkFree $ Create 4 (\i -> MkPure i)

    > blap5 :: Free (DataStoreF r Integer) r
    > blap5 = MkFree $ Create 5 (\i -> MkPure i)

    > hmm :: Free (DataStoreF t Integer) ()
    > hmm = do
    >   _ <- blap3
    >   _ <- blap4
    >   _ <- blap5
    >   return ()

    > hmm' :: Free (DataStoreF t Integer) ()
    > hmm' = blap3 >>= \_ ->
    >        blap4 >>= \_ ->
    >        blap5 >>= \_ ->
    >        return ()

    > hmm'' :: Free (DataStoreF t Integer) ()
    > hmm'' = blap3 >>= \_ ->
    >         MkPure ()

    > unMkFree (MkFree x) = x
    > unMkPure (MkPure x) = x

    > hmm''' :: Free (DataStoreF t Integer) ()
    > hmm''' = MkFree $ fmap (>>= (\_ -> (MkPure ()))) (unMkFree blap3)

    Need to check these with a print something function!

    > hmm'''' :: Free (DataStoreF t Integer) ()
    > hmm'''' = MkFree $ fmap (>>= (\_ -> (MkPure ()))) (Create 3 (\i -> MkPure i))

    > hmm''''' :: Free (DataStoreF t Integer) ()
    > hmm''''' = MkFree $ Create 3 $ (>>= (\_ -> (MkPure ()))) . (\i -> MkPure i)

    > hmm'''''' :: Free (DataStoreF t Integer) ()
    > hmm'''''' = MkFree $ Create 3 $ \i -> (MkPure i) >>= (\_ -> (MkPure ()))

    > hmm''''''' :: Free (DataStoreF t Integer) ()
    > hmm''''''' = MkFree $ Create 3 $ \i -> MkPure ()



    --------------------------------------------------------------------------------




        The module Control.Monad.Free provides liftF:

            ghci> :t liftF
            liftF :: (Functor f, MonadFree f m) => f a -> m a

        Let's try it out, using the id function (so a value's key is the
        value itself):

            ghci> :t Create 3 id
            Create 3 id :: Num v => DataStoreF k v k

            ghci> :t liftF $ Create 3 id
            liftF $ Create 3 id :: (Num v, MonadFree (DataStoreF a v) m) => m a

        So it becomes a monadic value in the general monad m. Can use it in
        do notation:

        > test1 :: DataStore Int Int ()
        > test1 = do
        >   _ <- liftF $ Create 3 id
        >   _ <- liftF $ Create 4 id
        >   _ <- liftF $ Create 5 id
        >   return ()

        What have we really created here? Let's write something to print a DataStore.

        > printDataStoreIntInt :: Show r => DataStore Int Int r -> String
        > printDataStoreIntInt (Free (Create v next)) = show v ++ "\n" ++ printDataStoreIntInt (next v)
        > printDataStoreIntInt (Pure x) = "pure: " ++ show x
        > printDataStoreIntInt _ = error "not implemented"

            ghci> putStrLn $ printDataStoreIntInt test1
            3
            4
            5
            pure: ()

        > liftF' :: (Functor f, MonadFree f m) => f a -> m a
        > liftF' = wrap . fmap return


        > test2 :: [Int]
        > test2 = do
        >   return (3 :: Int)
        >   return 4












        > create :: v -> DataStore i v i
        > create v = liftF $ Create v id

        > list :: DataStore i v [v]
        > list = liftF $ List id

        > retrieve :: i -> DataStore i v v
        > retrieve i = liftF $ Retrieve i id

        > update :: v -> DataStore i v ()
        > update v = liftF $ Update v ()

        > delete :: i -> DataStore i v ()
        > delete i = liftF $ Delete i ()

        > data Link = Link { linkId :: Maybe Int
        >                  , link :: T.Text
        >                  , rating :: Int
        >                  } deriving (Eq, Show)

        > setup :: DataStore Int Link ()
        > setup = do
        >   _ <- create (Link Nothing "Why dairy cows matter" 4)
        >   _ <- create (Link Nothing "Purify grass using dairy cows" 5)
        >   _ <- create (Link Nothing "Dairy cow transformers " 3)
        >   return ()

        > contains :: T.Text -> DataStore Int Link [Link]
        > contains t = do
        >   es <- list
        >   return $ filter (T.isInfixOf t . link) es

        > substitute :: T.Text -> T.Text -> DataStore Int Link ()
        > substitute from to = do
        >   es <- contains from
        >   forM_ es $ \(Link i l r) -> update (Link i (T.replace from to l) r)

        > runDB :: Int
        >       -> M.Map Int Link
        >       -> DataStore Int Link r
        >       -> (r, M.Map Int Link)

        > runDB _ m (Pure x) = (x, m)

        > runDB d m (Free (Create (Link Nothing l r) k)) =
        >  let
        >    i = (+ 1) . lastDef d . M.keys $ m
        >  in
        >   runDB d (M.insert i (Link (Just i) l r) m) (k i)

        > runDB d m (Free (Create (Link (Just i) _ _) k)) = runDB d m (k i)

        > runDB d m (Free (List k)) = runDB d m (k $ M.elems m)

        > runDB d m (Free (Retrieve i k)) = runDB d m (k $ m M.! i)

        > runDB d m (Free (Update v@(Link (Just _) _ _) k)) = runDB d (M.insert (fromJust . linkId $ v) v m) k

        > runDB d m (Free (Update (Link Nothing _ _) k)) = runDB d m k

        > runDB d m (Free (Delete i k)) = runDB d (M.delete i m) k


        > -- runDB 0 M.empty setup

        > showProgram :: Show r => DataStore Int Link r -> String
        > showProgram (Pure x) = show x
        > -- showProgram (Free (Create (Link Nothing l r) k)) =
        > -- showProgram (Free (Create (Link (Just i) _ _) k)) = runDB d m (k i)
        > -- showProgram (Free (List k)) = runDB d m (k $ M.elems m)
        > -- showProgram (Free (Retrieve i k)) = runDB d m (k $ m M.! i)
        > -- showProgram (Free (Update v@(Link (Just _) _ _) k)) = runDB d (M.insert (fromJust . linkId $ v) v m) k
        > -- showProgram (Free (Update (Link Nothing _ _) k)) = runDB d m k
        > -- showProgram (Free (Delete i k)) = runDB d (M.delete i m) k


