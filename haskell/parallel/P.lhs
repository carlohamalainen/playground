<p> A short note about using <a href="https://hackage.haskell.org/package/parallel-io">parallel-io</a>
to run shell commands in parallel from Haskell. If you want to try
out this blog post's Literate Haskell source then your best bet is to
compile in a sandbox which has various package versions fixed using
the <code>cabal.config</code> file (via the <code>cabal freeze</code>
command). </p>

<p> This is how to build the sandbox: </p>

<pre>
git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/parallel
rm -fr .cabal-sandbox cabal.sandbox.config dist # start fresh
cabal sandbox init
cabal install --haddock-hyperlink-source --dependencies-only
cabal install
cabal repl
</pre>

<p> Also, note the line</p>

<pre>
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
</pre>

<p> in <code>parallel.cabal</code>. Without those rtsopts options you would have to execute the binary
using <code>./P +RTS -N</code>. </p>

<p> Now, onto the actual blog post. First, a few imports to get us going. </p>

> module Main where

> import           Control.Concurrent
> import           Control.Monad
> import           Control.Concurrent.ParallelIO.Local
> import           Data.Traversable
> import qualified Pipes.ByteString                           as B
> import qualified Data.ByteString                            as BS
> import qualified Data.ByteString.Lazy                       as BSL
> import           Data.ByteString.Internal (w2c)
> import           System.Exit
> import           System.IO
> import           System.Process.Streaming

<p> In <a href="https://github.com/carlohamalainen/imagetrove-uploader">one of my work projects</a> I often need
to call legacy command line tools to process various imaging formats (DICOM, MINC, Nifti, etc). I used
to use a plain call to <code>createProcess</code> and then <code>readRestOfHandle</code> to read the stdout and stderr
but I <a href="http://carlo-hamalainen.net/blog/2014/8/28/reading-stdout-and-stderr-of-createprocess">discovered that it can deadlock</a> and a better approach is to use <a href="http://hackage.haskell.org/package/process-streaming">process-streaming</a>.
This is the current snippet that I use: </p>

> -- Copied from https://github.com/carlohamalainen/imagetrove-uploader/blob/master/src/Network/ImageTrove/Utils.hs
> -- Run a shell command, returning Right with stdout if the command exited successfully
> -- and Left with stderr if there was an exit failure.
> runShellCommand :: FilePath -> [String] -> IO (Either String String)
> runShellCommand cmd args = do
>
>     (exitCode, (stdOut, stdErr)) <- execute (pipeoe (fromFold B.toLazyM) (fromFold B.toLazyM)) ((proc cmd args))
>
>     return $ case exitCode of
>         ExitSuccess   -> Right $ map w2c $ BS.unpack $ BSL.toStrict stdOut
>         ExitFailure e -> Left $ "runShellCommand: exit status " ++ show e ++ " with stdErr: "
>                                                                 ++ (map w2c $ BS.unpack $ BSL.toStrict $ stdErr)

<p> Suppose we have a shell command that takes a while, in this case
because it's sleeping. Pretend that it's IO bound. </p>

> longShellCommand :: Int -> IO (Either String String)
> longShellCommand n = do
>   putStrLn $ "Running sleep command for " ++ show n ++ " second(s)."
>   runShellCommand "sleep" [show n ++ "s"]

<p> We could run them in order: </p>

> main1 :: IO ()
> main1 = do
>   -- Think of these as arguments to our long-running commands.
>   let sleepTimes = [1, 1, 1, 1]
>
>   forM_ sleepTimes longShellCommand

<p> In Haskell we can think of <code>IO</code> as a data type that describes
an IO action, so we can build it up using 'pure' code and then execute them later. To
make it a bit more explicit, here is a function for running an IO action: </p>

> runIO :: IO a -> IO a
> runIO x = do
>   result <- x
>   return result

<p> We can use it like this: </p>

<pre>
*Main> let action = print 3 -- pure code, nothing happens yet
*Main> runIO action         -- runs the action
3
</pre>

<p> And we can rewrite <code>main1</code> like this: </p>

> main2 :: IO ()
> main2 = do
>   let sleepTimes = [1, 1, 1, 1]
>
>   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
>
>   forM_ actions runIO

<p> As an aside, <code>runIO</code> is equivalent to <code>liftM id</code> (see <a href="http://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Monad.html#v:liftM">Control.Monad</a> for info about <code>liftM</code>). </p>

<p> Now, imagine that you had a lot of these shell commands to execute and wanted a pool of, say, 4 workers.
The <a href="https://hackage.haskell.org/package/parallel-io">parallel-io</a> package provides <code>withPool</code>
which can be used like this: </p>

<pre>
withPool 4 $ \pool -> parallel_ pool [putStrLn "Echo", putStrLn " in parallel"]
</pre>

<p> Note that the IO actions (the <code>putStrLn</code> fragments) are provided in a list. A list of IO actions. So we
can run our shell commands in parallel like so: </p>


> main3 :: IO ()
> main3 = do
>   let sleepTimes = [1, 1, 1, 1]
>
>   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
>
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   withPool 4 $ \pool -> parallel_ pool actions

<p> If we did this a lot we might define our own version of <code>forM_</code> that uses <code>withPool</code>: </p>

> parForM_ :: Int -> [IO a] -> IO ()
> parForM_ nrWorkers tasks = withPool nrWorkers $ \pool -> parallel_ pool tasks

> main4 :: IO ()
> main4 = do
>   let sleepTimes = [1, 1, 1, 1]
>
>   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
>
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   parForM_ 4 actions

<p> Here is another example of building up some IO actions in pure form and then executing them later. Imagine that
instead of a list of Ints for the sleep times, we have some actual sleep times and others that represent an error case.
An easy way to model this is using <a href="http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Either.html#t:Either">Either</a>, which by convention has the erroneous values in the <code>Left</code> and correct values in the <code>Right</code>. </p>

> main5 :: IO ()
> main5 = do
>   let sleepTimes = [Right 1, Left "something went wrong", Right 2, Right 3]
>
>   let actions  = map (traverse longShellCommand) sleepTimes :: [IO (Either [Char] (Either String String))]
>       actions' = map (fmap join) actions                    :: [IO (Either [Char] String)]
>
>   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
>
>   parForM_ 4 actions'

<p> In <code>main5</code> we define <code>actions</code> by mapping a function over the sleep times, which are are now
of type <code>Either String Int</code>. We can't apply <code>longShellCommand</code> directly because it
expects an <code>Int</code>, so we use <code>traverse longShellCommand</code> instead (see <a href="http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Traversable.html#v:traverse">Data.Traversable</a> for the definition of <code>traverse</code>). </p>

<p> Next, the Either-of-Either is a bit clunky but we can mash them together using <code>join</code>. Here we have
to use <code>fmap</code> because we have list elements of type <code>IO (Either [Char] String)</code>, not <code>Either [Char] String</code> as <code>join</code> might expect. </p>

<p> One topic that I haven't touched on is dealing with asynchronous exceptions. For this, have a read of <a href="https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions">Catching all exceptions</a> from Snoyman and also <a href="http://hackage.haskell.org/package/enclosed-exceptions">enclosed-exceptions</a>. Also, <a href="http://chimera.labs.oreilly.com/books/1230000000929/ch13.html">Chapter 13</a> of <a href="http://chimera.labs.oreilly.com/books/1230000000929">Parallel and Concurrent Programming in Haskell</a> shows how to use the handy <a href="https://hackage.haskell.org/package/async">async</a> package. </p>


> -- Run all of the mains.
> main :: IO ()
> main = do
>
>   print "main1"
>   main1
>
>   print "main2"
>   main2
>
>   print "main3"
>   main3
>
>   print "main4"
>   main4
>
>   print "main5"
>   main5
