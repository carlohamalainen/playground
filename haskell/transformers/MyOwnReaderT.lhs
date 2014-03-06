<p> Note to self on constructing a monad transformer. In a way this follows on from
the earlier post
<a href="http://carlo-hamalainen.net/blog/2014/1/2/applicatives-compose-monads-do-not">Applicatives compose, monads do not</a>. Literate Haskell source
for this post is available here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/transformers">https://github.com/carlohamalainen/playground/tree/master/haskell/transformers</a>. </p>

> {-# LANGUAGE ScopedTypeVariables, InstanceSigs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
>
> module MyOwnReaderT where
>
> import Control.Monad
>
> hole = undefined
> data Hole = Hole

<h2> The Reader Monad </h2>

<p> A <code>Reader</code> is a data type that encapsulates an
environment. The <code>runReader</code> function takes an environment
(a <code>Reader</code>) and runs it, producing the result of type
<code>a</code>. </p>

> newtype Reader r a = Reader { runReader :: r -> a }

<p> Note that with record syntax, the type of <code>runReader</code> is actually </p>

<pre>
runReader :: Reader r a -> (r -> a)
</pre>

<p> Reader that increments its environment value, returning the same type: </p>

> reader1 :: Num r => Reader r r
> reader1 = Reader $ \r -> r + 1

<p> Reader that converts its environment value into a string: </p>

> reader2 :: Show r => Reader r String
> reader2 = Reader $ \r -> "reader2: " ++ (show r)

<p> "Run" the reader using <code>runReader</code>: </p>

< ghci> runReader reader1 100
< 101
< ghci> runReader reader2 100
< "reader2: 100"

<p> There's nothing magic about <code>runReader</code>; it is just taking the function out
of the data type. We can do it manually ourselves: </p>

> runReader' :: Reader r a -> (r -> a)
> runReader' (Reader f) = f

< ghci> runReader' reader1 100
< 101

<p> Next, make <code>Reader</code> an instance of <code>Monad</code>: </p>

> instance Monad (Reader r) where
>     return :: a -> Reader r a
>     return a = Reader $ \_ -> a
>
>     (>>=) :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
>     m >>= k  = Reader $ \r -> runReader (k (runReader m r :: a) :: Reader r b) r

<p> The definition of <code>>>=</code>
is relatively easy to work out using
<a href="http://matthew.brecknell.net/post/hole-driven-haskell/">hole
driven development</a>. </p>

<p> Example usage: </p>

> eg1 :: Reader Int String
> eg1 = Reader id >>= \e -> return $ "hey, " ++ show e

<p> Or in the more readable <code>do</code> notation: </p>

> eg1' :: Reader Int String
> eg1' = do e <- Reader id
>           return $ "hey, " ++ show e

< ghci> runReader eg1' 100
< "hey, 100"

<p> Note that we use <code>id</code> to produce a <code>Reader</code> that just passes its environment argument along as the output. See
<code>readerAsk</code> later for the equivalent situation which uses <code>return</code> for <code>MonadReader</code>. </p>

<p> Since <code>[]</code> is an instance of <code>Monad</code> we can also do things like this: </p>

> eg1'' :: Reader Int String
> eg1'' = do e <- Reader (return :: Int -> [] Int)
>            return $ "hey, " ++ show e

< ghci> runReader eg1'' 100
< "hey, [100]"

<h2> Reader Transformer </h2>

<p> We'd like to use the <code>Reader</code> in conjunction with other monads, for example running <code>IO</code> actions but
having access to the reader's environment. To do this we create a transformer, which we'll call <code>ReaderT</code>:

> newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

<p> The <code>r</code> parameter is the reader environment as
before, <code>m</code> is the monad (for example <code>IO</code>),
and <code>a</code> is the result type, as before. Again, note the actual
type of <code>runReaderT</code>: </p>

< runReaderT :: ReaderT r m a -> (r -> m a)

<p> It takes a <code>ReaderT</code> and provides us with a function that takes a reader environment
of type <code>r</code> and produces a monadic value. Following from the
<code>Monad</code> instance declaration for <code>Reader</code> it is
straightforward to write the definition for <code>ReaderT</code>. </p>

> instance Monad m => Monad (ReaderT r m) where
>     return a = ReaderT $ \r -> return a
>
>     (>>=) :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
>     m >>= k  = ReaderT $ \r -> do let a' = runReaderT m r :: m a
>                                   a'' <- a'
>                                   runReaderT (k a'') r :: m b

<p> With a <code>ReaderT</code> as the outer monad, we would like to "lift" a monadic computation into the reader. A monadic action doesn't know anything
about the reader environment, so to lift the monadic value we
just create a <code>ReaderT</code> with a function that ignores the environment: </p>

> liftReaderT :: m a -> ReaderT r m a
> liftReaderT m = ReaderT (\_ -> m)

<p> Example usage: </p>

> egLift :: ReaderT Int IO ()
> egLift = do e <- ReaderT (return :: Int -> IO Int) -- This is similar to "Reader id" earlier.
>             liftReaderT $ print "boo"
>             liftReaderT $ print $ "value of e: " ++ show e

<p> Note the type of <code>return</code> on the first line of <code>egLift</code>. In this context,
<code>return :: a -> m a</code> is the equivalent of <code>id :: a -> a</code> from the earlier
<code>Reader</code> example.

< ghci> runReaderT egLift 100
< "boo"
< "value of e: 100"

<p> More generally, let's name the "ask" function: </p>

> readerAsk :: (Monad m) => ReaderT r m r
> readerAsk = ReaderT return

<p> If we want to modify the environment, we use
<code>withReaderT</code> which takes as its first parameter a
function to modify the environment. Note that the result is of type
<code>ReaderT r' m a</code> so the function is of type <code>r' ->
r</code> which modifies the supplied reader of type <code>ReaderT r
m a</code>. </p>

> withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
> withReaderT f rt = ReaderT $ (runReaderT rt) . f

<p> Lastly, it is convenient to apply a function to the current environment. </p>

> readerReader :: Monad m => (r -> a) -> ReaderT r m a
> readerReader f = ReaderT $ \r -> return (f r)

<p> This is almost the same as <code>readerAsk</code> except
that we create a reader that returns <code>f r</code> instead of
<code>f</code>. In other words: </p>

> readerAsk' :: (Monad m) => ReaderT r m r
> readerAsk' = readerReader id

<p> Finally, we collect the functions <code>readerAsk</code>,
<code>withReader</code>, and <code>readerReader</code> in a type class
<code>MonadReader</code> and give them more general names: </p>

> class (Monad m) => MonadReader r m | m -> r where
>     -- Retrieve the monad environment.
>     ask   :: m r
>
>     -- Execute the computation in the modified environment.
>     local :: (r -> r) -> m a -> m a
>
>     -- Retrieves a function of the current environment.
>     reader :: (r -> a) -> m a

<p> An instance declaration for our <code>ReaderT</code> type: <p>

> instance Monad m => MonadReader r (ReaderT r m) where
>     ask    = readerAsk    :: ReaderT r m r
>     local  = withReaderT  :: (r -> r) -> ReaderT r m a -> ReaderT r m a
>     reader = readerReader :: (r -> a) -> ReaderT r m a

<p> Now we can write fairly succinct code as follows. Use the <code>IO</code> monad as the inner monad in a <code>ReaderT</code>,
with an <code>Int</code> environment and <code>String</code> result type. </p>

> eg2 :: ReaderT Int IO String
> eg2 = do
>     -- Get the reader environment.
>     e <- ask :: ReaderT Int IO Int
>
>     -- Run an IO action; we have to use liftReaderT since we are currently
>     -- in the ReaderT monad, not the IO monad.
>     liftReaderT $ print $ "I'm in the eg2 function and the environment is: " ++ (show e)
>
>     -- Final return value, a string.
>     return $ "returned value: " ++ show e

< ghci> result <- runReaderT eg2 100
< "I'm in the eg2 function and the environment is: 100"
<
< ghci> result
< "returned value: 100"

<p> All of the above is available from <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader.html">Control.Monad.Reader</a>
and <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Trans.html">Control.Monad.Trans</a>. </p>

<h2> StateT, ReaderT, and ask </h2>

<p> The <a href="http://www.haskell.org/haskellwiki/State_Monad">State</a> monad encapsulates a modifiable state. It has a transformer
<code>StateT</code> as one would expect. Yet we are able to call <code>ask</code> inside
a <code>StateT</code> monad. Here is an example (the raw code is <a href="https://github.com/carlohamalainen/playground/blob/master/haskell/transformers/Ask.hs">here</a>): </p>

< import Control.Monad.Reader
< import Control.Monad.State
<
< inside0 :: ReaderT String IO Float
< inside0 = do
<     e <- ask :: ReaderT String IO String
<
<     liftIO $ putStrLn $ "inside0, e: " ++ show e
<
<     return 1.23
<
< inside1 :: StateT [Int] (ReaderT String IO) Float
< inside1 = do
<     e <- ask :: StateT [Int] (ReaderT String IO) String
<     s <- get :: StateT [Int] (ReaderT String IO) [Int]
<
<     liftIO $ putStrLn $ "inside1, e: " ++ show e
<     liftIO $ putStrLn $ "inside1, s: " ++ show s
<
<     put [1, 1, 1]
<
<     return 1.23
<
< run0 :: IO ()
< run0 = do let layer1 = runReaderT inside0 "reader environment, hi"
<
<           result <- layer1
<
<           print $ "result: " ++ show result
<
< run1 :: IO ()
< run1 = do let layer1 = runStateT inside1 [0]
<               layer2 = runReaderT layer1 "reader environment, hi"
<
<           (result, finalState) <- layer2
<
<           print $ "final state: " ++ show finalState
<
<           print $ "result: " ++ show result


<p> The function
<code>inside0</code> has the <code>IO</code> monad nested inside a <code>Reader</code>, while
</code>inside1</code> has a <code>StateT</code> with the <code>ReaderT</code> inside. Yet
in both we can write <code>e <- ask</code>. </p>

<p> Inspecting the types using <a href="https://github.com/eagletmt/ghcmod-vim">ghcmod-vim</a> we find that </p>

<   -- in inside0
<   e <- ask :: ReaderT String IO String
<
<   -- in inside1
<   e <- ask :: StateT [Int] (ReaderT String IO) String

<p> so there must be a type class that provides the <code>ask</code> function for
<code>StateT</code>.

<p> First, inspect the type of <code>ask</code> using ghci (here
we are using the definitions from <code>Control.Monad.Reader</code>
and <code>Control.Monad.State</code>, not the implementation in this
file). </p>

<pre>
ghci> :t ask
ask :: MonadReader r m => m r
</pre>

<p> So <code>StateT</code> must have a <code>MonadReader</code> instance. Confirm this with <code>:i</code>: </p>

<pre>
ghci> :i StateT
(lots of stuff)
instance MonadReader r m => MonadReader r (StateT s m)
  -- Defined in `Control.Monad.Reader.Class'
(lots of stuff)
</pre>

<p> Looking in <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader-Class.html">Control.Monad.Reader.Class</a> we find: </p>

< instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
<     ask   = lift ask
<     local = Lazy.mapStateT . local
<     reader = lift . reader

<p> The <code>lift</code> function comes from <a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html">Monad.Trans.Class</a>,
and looking there we see: </p>

< class MonadTrans t where
<     -- | Lift a computation from the argument monad to the constructed monad.
<     lift :: Monad m => m a -> t m a

<p> So actually we are after the <code>MonaTrans</code> type
class. Again looking at <code>:i</code> on <code>StateT</code> we see: </p>


<pre>
ghci> :i StateT
instance MonadTrans (StateT s)
(lots of stuff)
  -- Defined in `Control.Monad.Trans.State.Lazy'
(lots of stuff)
</pre>

<p> So off we go to <a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-State-Lazy.html">Control.Monad.Trans.State.Lazy</a> where we finally get the answer: </p>

< instance MonadTrans (StateT s) where
<     lift m = StateT $ \s -> do
<         a <- m
<         return (a, s)

<p> This shows that for <code>StateT</code>, the <code>lift</code>
function takes a monadic action and produces a state transformer that
takes the current state, runs the action, and returns the result of
the action along with the unmodified state. This makes sense in that
the underlying action should not modify the state. (There are some
<a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html">laws</a> that monad transformers must satisfy.) </p>

<p> If we did not have the <code>MonadTrans</code> type class then we would have to embed
the <code>ask</code> call manually: </p>

< inside1' :: StateT [Int] (ReaderT String IO) Float
< inside1' = do
<     e <- StateT $ \s -> do a <- ask
<                            return (a, s)
<
<     s <- get :: StateT [Int] (ReaderT String IO) [Int]
<
<     liftIO $ putStrLn $ "inside1, e: " ++ show e
<     liftIO $ putStrLn $ "inside1, s: " ++ show s
<
<     put [1, 1, 1]
<
<   return 1.23

<p> Obviously this is laborious and error-prone. In this case, Haskell's type class system lets
us implement a few classes so that <code>ask</code>, <code>get</code>, <code>put</code>, etc, can be used seamlessly no matter which monad transformer we are in. </p>

<p> The downside is that reading Haskell code can be nontrivial. In our case we had to follow
a trail through a few files to see where <code>ask</code> was actually implemented, and finding
the right definition relied on us being able to infer the correct types of certain sub-expressions. </p>

<p> Personally I am finding more and more that plain vim and ghci does not cut it for Haskell development, and something richer like <a href="https://github.com/eagletmt/ghcmod-vim">ghcmod-vim</a> is a real necessity. Shameless self plug: <a href="https://github.com/carlohamalainen/ghc-imported-from">ghc-imported-from</a> is also very useful :-) </p>





