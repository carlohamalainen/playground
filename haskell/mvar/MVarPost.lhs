<p> A short note about using MVars in Haskell. Source is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/mvar">https://github.com/carlohamalainen/playground/tree/master/haskell/mvar</a>. </p>

<p> Unlike earlier blog posts, this one should be built using <a href="https://github.com/commercialhaskell/stack">Stack</a>.
Something like: </p>

<pre>
git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/mvar
stack build
</pre>

<p> Then use <code>stack ghci</code> instead of <code>cabal repl</code>. The main executable is <code>.stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/mvar-exe/mvar-exe</code>. </p>

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Main where
>
> import Control.Concurrent
> import Control.Monad
> import Control.Concurrent.ParallelIO.Local
> import System.IO

<p> Here is the situation: we have a function that makes a call to some
restricted resource, say a network API, and we would like calls to this
API from our application to be serialized across multiple threads.
For the purposes of this blog post, here is a dummy function that
sleeps a bit and returns <code>x + 1</code>. Pretend that it's calling
a magical API on the network somewhere. </p>

> getExpensiveThing :: Int -> IO Int
> getExpensiveThing x = do
>   threadDelay $ 1 * 10^6
>   return $ x + 1

<p> We have a general task that makes use of the expensive resource: </p>

> doThings :: Int -> IO ()
> doThings tid = do
>   x <- getExpensiveThing tid
>   putStrLn $ "doThings: thread " ++ show tid ++ " got " ++ show x

<p> At the top level we need to run a few <code>doThings</code> in parallel: </p>

> main0 :: IO ()
> main0 = do
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   let tasks = map doThings [1..5]
>
>   withPool 4 $ \pool -> parallel_ pool tasks

<p> The problem with <code>main0</code> is that the calls to <code>getExpensiveThing</code>
can happen simultaneously, so we need to use some kind of thread synchronisation primitive. I initially thought
that I'd have to use a semaphore, a queue, or something fancy, but an <a href="http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Concurrent-MVar.html#t:MVar">MVar</a> can do the trick. </p>

<p> We only need three operations on <code>MVar</code>: </p>

<p> Use <code>newEmptyMVar</code> to create a new <code>MVar</code> which is initially empty: </p>

< newEmptyMVar :: IO (MVar a)

<p> Use <code>takeMVar</code> to get the contents of the <code>MVar</code>. If the <code>MVar</code> is empty, <code>takeMVar</code> will wait until it is full. </p>

< takeMVar :: MVar a -> IO a

<p> Finally, use <code>putMVar</code> to put a value into an <code>MVar</code>. If the <code>MVar</code> is full, then <code>putMVar</code> will wait
until the <code>MVar</code> is empty. If multiple threads are blocked on an <code>MVar</code>, they are woken up in FIFO order. </p>

< putMVar :: MVar a -> a -> IO ()

<p> So what we can do is have <code>getExpensiveThing</code> use <code>takeMVar</code> to block until a worker requires a value. The return value can be passed back via another <code>MVar</code>, which the worker is itself waiting on. The data type <code>MVar</code> is polymorphic in its type parameter, so there is no trouble
in having an <code>MVar</code> of an <code>MVar</code>, or an <code>MVar</code> of a tuple containing another <code>MVar</code>, and so on. This is what we'll use to represent a blocking action with input value of type <code>a</code> and output value of type <code>b</code>: </p>

> data InOut a b = InOut (MVar (a, MVar b))

<p> The outer <code>MVar</code> wraps a tuple, where the first component is the raw input value of type <code>a</code>, and the
second component is the <code>MVar</code> in which the result value will be passed back. Here is the new <code>getExpensiveThing</code>: </p>

> getExpensiveThing' :: InOut Int Int -> IO ()
> getExpensiveThing' (InOut io) = forever $ do
>   ((input :: Int), (output :: MVar Int)) <- takeMVar io
>   threadDelay $ 1 * 10^6
>   putMVar output (input + 1)

<p> The output <code>MVar</code> is contained inside the
top level <code>MVar</code>. This way, <code>getExpensiveThing'</code>
has a unique channel back to the calling function. I used <code>ScopedTypeVariables</code> to be able to write the types of <code>input</code> and <code>output</code> inline, but this is just for clarity in this blog post. Also note that <code>getExpensiveThing'</code> runs forever using <a href="http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html#v:forever">forever</a> from <a href="http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html">Control.Monad</a>. </p>

<p> Here is the updated <code>doThings</code> that uses the <code>MVar</code> to communicate with <code>getExpensiveThing'</code>: </p>

> doThings' :: InOut Int Int -> Int -> IO ()
> doThings' (InOut io) tid = do
>   result <- newEmptyMVar      -- For our result.
>   putMVar io (tid, result)    -- Send our input (tid) and the result MVar.
>   x <- takeMVar result        -- Get the value from the result MVar.
>
>   putStrLn $ "doThings': thread " ++ show tid ++ " got " ++ show x

<p> Finally, <code>main</code> needs a top-level <code>MVar</code> which is the first parameter to <code>doThings'</code>
and a forked thread to run <code>getExpensiveThing'</code>: </p>

> main :: IO ()
> main = do
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   topMVar <- newEmptyMVar
>
>   _ <- forkIO $ getExpensiveThing' (InOut topMVar)
>
>   let tasks = map (doThings' (InOut topMVar)) [1..5]
>
>   withPool 4 $ \pool -> parallel_ pool tasks

<p> Now each evaluation of <code>threadDelay</code> (the sensitive bit of code that represents a call to a resource) happens sequentially</code> although the order is nondeterministic. </p>

<pre>
$ stack build && .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/mvar-exe/mvar-exe
doThings': thread 1 got 2
doThings': thread 5 got 6
doThings': thread 2 got 3
doThings': thread 4 got 5
doThings': thread 3 got 4
</pre>

<p> Just for fun, let's make some helper functions to make calling a special worker via an <code>MVar</code> a bit cleaner. In general,
calling a worker requires creating a results <code>MVar</code>, pushing the input and results <code>MVar</code> to the <code>InOut</code> <code>MVar</code>,
and finally taking the result. </p>

> callWorker :: InOut a b -> a -> IO b
> callWorker (InOut m) a = do
>     result <- newEmptyMVar
>     putMVar m (a, result)
>     takeMVar result

<p> To save ourselves having to fork a worker, we can write a
combinator that takes a worker and an action and runs the action with
access to the newly created <code>MVar</code>: </p>

> withWorker :: (InOut a b -> IO ()) -> (InOut a b -> IO c) -> IO c
> withWorker worker action = do
>   m <- newEmptyMVar
>   let io = InOut m
>   _ <- forkIO $ worker io
>   action io

<p> Now <code>doThings''</code> is a bit shorter, at the expense of not knowing (at a glance) what the <code>io</code> thing is going to do. </p>

> doThings'' :: InOut Int Int -> Int -> IO ()
> doThings'' io tid = do
>   x <- callWorker io tid
>
>   putStrLn $ "doThings'': thread " ++ show tid ++ " got " ++ show x

<p> Finally, <code>main'</code> is largely unchanged except for <code>withWorker</code> at the top level. </p>

> main' :: IO ()
> main' = withWorker getExpensiveThing' $ \io -> do
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   let tasks = map (doThings'' io) [1..5]
>
>   withPool 4 $ \pool -> parallel_ pool tasks

<p> Running <code>main'</code>: </p>

<pre>
*Main> main'
doThings'': thread 2 got 3
doThings'': thread 3 got 4
doThings'': thread 4 got 5
doThings'': thread 1 got 2
doThings'': thread 5 got 6
</pre>

