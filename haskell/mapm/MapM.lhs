<p> Not to self about <code>mapM</code>. Is it lazy? Sort of.</p>

<p> Literate source is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/mapm">https://github.com/carlohamalainen/playground/tree/master/haskell/mapm</a>. </p>

<p> First, some imports: </p>

> {-# LANGUAGE OverloadedStrings, InstanceSigs #-}
>
> import Control.Applicative
> import Control.Monad
> import qualified Data.ByteString as B
> import Data.ByteString.Internal (w2c)
> import Data.Either

<p> I recently wrote some code using <a href="http://hackage.haskell.org/package/wreq">wreq</a>
that seemed to use much more memory than I thought it should. The problem turned out not to be
with wreq but with the way that I was using <code>mapM</code>. An equivalent snippet of code is: </p>

> main1 = do
>   firstFile <- head <$> mapM B.readFile (take 100000 $ repeat "MapM.lhs")
>   print $ B.length firstFile

<p> I reasoned that <code>mapM</code> would construct its result
lazily, then <code>head</code> would force evaluation of just the
first element of the list. This isn't the case, as <a href="http://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy">explained here</a>.
The function <code>mapM</code> is basically equivalent to this: </p>

> mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
> mapM' m [] = return []
> mapM' m (x:xs) = do
>   x' <- m x
>   xs' <- mapM' m xs
>   return (x':xs')

<p> So the monadic action <code>m</code> is evaluated to build up the list elements. </p>

<p> One of the answers on the StackOverflow page says to use a step by step series to only evaluate the
bits that are required:

< data Stream m a = Nil | Stream a (m (Stream m a))

<p> GHC 7.8.3 comes with <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/src/Stream.html">Stream</a> defined as: </p>

> -- In GHC 7.8.3:
> newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }

<p> The idea is that it represents a sequence of monadic actions. A <code>Left</code> is a final value
of type <code>b</code>, while <code>Right (a, Stream m a b)</code> represents an intermediate value
of type <code>a</code> along with the remaining stream. </p>

<p> The <code>Monad</code> instance is fairly straightforward. The <code>return</code> function turns a plain value
into a final value (hence the <code>Left</code>), and the bind either stops with the final value or produces the new value
along with the next stream. </p>

> instance Monad m => Monad (Stream m a) where
>   return a = Stream $ return $ Left a
>   Stream m >>= k = Stream $ do
>                       r <- m
>                       case r of
>                           Left b         -> runStream $ k b
>                           Right (a, str) -> return $ Right (a, str >>= k)

<p> There are also instances for <code>Functor</code> and <code>Applicative</code> but we don't need them here. </p>

<p> A handy function is <code>liftIO</code> which turns a normal monadic action into
a stream: </p>

> liftIO :: IO a -> Stream IO b a
> liftIO io = Stream $ io >>= return . Left

<p> It just runs the <code>io</code> action, and pipes it to a <code>Left</code> and then returns it in a <code>Stream</code>.

> readFileS :: FilePath -> Stream IO b B.ByteString
> readFileS f = liftIO $ B.readFile f

<p> To use <code>readFileS</code> we wrap it with <code>runStream</code>: </p>

<pre>
*Main> Left x <- runStream $ readFileS "MapM.lhs"
*Main> print $ B.length x
4243
</pre>

<p> So we can produce final values, but what about intermediate ones? This is what <code>yield</code> does: </p>

> yield :: Monad m => a -> Stream m a ()
> yield a = Stream $ return $ Right $ (a, return ())

<p> At this point we have no idea about the remaining stream, so we return the unit <code>()</code>. </p>

<p> For testing the code here we'll take the definition of <code>collect</code> from Stream as well. It just walks
through the entire Stream and collects the values, ignoring the final unit value. </p>

> collect :: Monad m => Stream m a () -> m [a]
> collect str = go str []
>  where
>   go str acc = do
>     r <- runStream str
>     case r of
>       Left () -> return (reverse acc)
>       Right (a, str') -> go str' (a:acc)

<p> Now we can try out <code>yield</code> using monadic notation: </p>

> yield123 :: Stream IO Int ()
> yield123 = do
>   yield 1
>   yield 2
>   yield 3

<pre>
*Main> collect yield123
[1,2,3]
</pre>

<p> We can mix normal Haskell control structures like if/then/else into the monadic notation: </p>

> yieldEvens :: Int -> Stream IO Int ()
> yieldEvens n = if n > 10
>                   then return ()
>                   else do yield n
>                           yieldEvens $ n + 2

<pre>
*Main> collect $ yieldEvens 0
[0,2,4,6,8,10]
</pre>

<p> We could read some files using our <code>readFileS</code> function and yield the results: </p>

> readAFewFiles :: Stream IO B.ByteString ()
> readAFewFiles = do
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield

<pre>
*Main> length <$> collect readAFewFiles
5
</pre>

<p> We can generalise this to apply a monadic function to a list of arguments, which is basically what <code>mapM</code> does:

> streamMapM :: (String -> IO B.ByteString) -> [String] -> Stream IO B.ByteString ()
> streamMapM _ [] = return ()
> streamMapM f (a:as) = do
>   (liftIO $ f a) >>= yield
>   streamMapM f as

<p> And we can even make an infinite stream: </p>

> readForever :: Stream IO B.ByteString ()
> readForever = streamMapM B.readFile (repeat "MapM.lhs")

<p> Take from a stream and a definition of head for a stream: </p>

> takeStream :: Integer -> Stream IO a () -> IO [a]
> takeStream n str = go str [] n
>  where
>   go str acc n = do
>     if n <= 0 then return acc
>               else do r <- runStream str
>                       case r of
>                           Left ()         -> return (reverse acc)
>                           Right (a, str') -> go str' (a:acc) (n - 1)
>
> headStream :: Stream IO a () -> IO (Maybe a)
> headStream str = do
>   h <- takeStream 1 str
>   return $ case h of
>               [h'] -> Just h'
>               _    -> Nothing

<p> So we can efficiently take the head of the stream without evaluating the entire thing: </p>

<pre>
*Main> (fmap B.length) <$> headStream readForever
Just 5917
</pre>


<p> I should point out that the example of reading a file a bunch of
times could be achieved without <code>Stream</code> just by storing a list
of the monadic actions, and then evaluating the one that we want: </p>

> listOfActions :: [IO B.ByteString]
> listOfActions = repeat $ B.readFile "MapM.lhs"

<p> which can be used as follows: </p>

<pre>
*Main> B.length <$> (head $ listOfActions)
6455
</pre>

<p> The difference is that the list is somewhat static, in that we can't mix
control structures into it as we can do with <code>Stream</code>. </p>

<p> Interestingly, the definition for <code>Stream</code> looks very similar
to the definition for <code>Free</code>, which I used in an <a href="/blog/2014/6/7/notes-on-free-monads">earlier post about free monads</a>:

< data Stream m a = Nil      | Stream a (m (Stream m a))

> data Free   f r = MkPure r | MkFree   (f (Free   f r))

<p> Here's one way to encode <code>Stream</code>-like behaviour using free monads. I define
two actions, yield and final. The yield action stores an input value of
type <code>a</code>, a monadic function <code>a -> IO b</code>, and the
rest of the structure, which turns out to be conveniently represented as
a function <code>b -> k</code>. Being a function of <code>b</code> lets the rest
of the structure depend on the result at the current node in the structure.
The final action just stores the value and monadic
action, and is a terminal node in the free monad. </p>

> data StreamF a b k = Yield a (a -> IO b) (b -> k)
>                    | Final a (a -> IO b)

<p> For convenience, <code>Command</code> is a simpler type signature:</p>

> type Command a b k = Free (StreamF a b) k

<p> As in my earlier post, we need instances for <code>Functor</code>
and <code>Monad</code>. They are fairly straightforward: </p>

> instance Functor (StreamF a b) where
>   fmap f (Yield a io k) = Yield a io (f . k)
>   fmap _ (Final a io)   = Final a io
>
> instance (Functor f) => Monad (Free f) where
>     return :: a -> Free f a
>     return x = MkPure x
>
>     (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>     (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
>     (MkPure r) >>= f = f r

<p> Here are two helpers to make <code>Command</code>'s monadic usage easier: </p>

> -- Lift an IO action to a final Command.
> finalF :: a -> (a -> IO b) -> Command a b r
> finalF a io = MkFree $ Final a io
>
> -- Lift an IO action to a Command that yields the value
> -- and continues.
> yieldF :: a -> (a -> IO b) -> Command a b b
> yieldF a io = MkFree $ Yield a io (\b -> MkPure b)

<p> To run a <code>Command</code> we walk its structure recursively and
run the IO actions as needed: </p>

> runCommand :: (Show a, Show b, Show r) => Command a b r -> IO ()
>
> runCommand (MkFree (Final a io)) = do
>   putStrLn $ "Final " ++ show a
>   x <- io a
>   putStrLn $ "Produced the value: " ++ show x
>
> runCommand (MkFree (Yield a io next)) = do
>   b <- io a
>   putStrLn $ "Yield: computed value: " ++ show b
>   runCommand (next b)
>
> runCommand (MkPure x) = putStrLn $ "MkPure: " ++ show x

<p> As with <code>Stream</code>, we can mix control structures with the creation of the free monad: </p>

> exampleCommand :: Command FilePath String String
> exampleCommand = do
>   x <- yieldF "hello1.txt" readFile
>   y <- if x == "hello1\n"
>           then yieldF "hello2.txt" readFile
>           else finalF "hello3.txt" readFile
>   return y

<p> For example: </p>

<pre>
Yield: computed value: "hello1\n"
Yield: computed value: "hello2\n"
MkPure: "hello2\n"
</pre>

<p> Taking the head of a <code>Command</code> is straightforward
using the definition of <code>runCommand</code>: </p>

> headCommand :: Command a r r -> IO r
> headCommand (MkFree (Final a io  )) = io a
> headCommand (MkFree (Yield a io _)) = io a
> headCommand (MkPure x)              = return x

<p> Here it is in action: </p>

<pre>
*Main> :t headCommand exampleCommand
headCommand exampleCommand :: IO String

*Main> headCommand exampleCommand
"hello1\n"
</pre>

<p> To finish things off, here are versions of <code>take</code> and <code>mapM</code>
on <code>Command</code>: </p>

> runOneCommand :: Command t t () -> IO (Either () (t, Command t t ()))
>
> runOneCommand (MkFree (Final a io)) = do
>   x <- io a
>   return $ Right (x, MkPure ())
>
> runOneCommand (MkFree (Yield a io next)) = do
>   b <- io a
>   return $ Right (b, next b)
>
> runOneCommand (MkPure ()) = Left <$> return ()
>
> takeCommand :: Integer -> Command t t () -> IO [t]
> takeCommand n str = go str [] n
>  where
>   go str acc n = do
>     if n <= 0 then return acc
>               else do r <- runOneCommand str
>                       case r of
>                           Left ()         -> return $ reverse acc
>                           Right (a, str') -> go str' (a:acc) (n - 1)
>
> commandMapM :: (a -> IO a) -> [a] -> Command a a ()
> commandMapM _ [] = MkPure ()
> commandMapM f (a:as) = do
>   yieldF a f
>   commandMapM f as

<p> It works like the <code>Stream</code> example: </p>

> takeCommandExample = (fmap B.length) <$> (takeCommand 3 $ commandMapM readFileBB (take 100000 $ repeat "MapM.lhs")) >>= print
>  where
>     -- Since B.readFile :: String -> B.ByteString
>     -- we have to write this wrapper so that the input
>     -- and result types match, as required by the
>     -- restriction "Command t t ()" in the signature
>     -- for takeCommand.
>     readFileBB :: B.ByteString -> IO B.ByteString
>     readFileBB = B.readFile . (map w2c) . B.unpack

<p> There we go: </p>

<pre>
*Main> takeCommandExample
[11241,11241,11241]
</pre>



