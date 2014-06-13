-- Adapted from http://chimera.labs.oreilly.com/books/1230000000929/ch12.html#sec_server-trivial

import Network
import Control.Monad
import Control.Concurrent (forkIO, forkFinally)
import System.IO
import Text.Printf
import Control.Exception

-- Burn up the CPU.
fib n = _fib !! n
    where _fib = 0 : 1 : zipWith (+) _fib (tail _fib)

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    loop
  where
    loop = do
        line <- hGetLine h
        if line == "end"
            then hPutStrLn h "Thanks for using the fib service."
            else do hPutStrLn h (show (fib (read line :: Int)))
                    loop

port :: Int
port = 44444

main = withSocketsDo $ do
    sock <- listenOn (PortNumber (fromIntegral port))

    printf "Listening on port %d\n" port

    forever $ do
        (handle, host, port) <- accept sock
        printf "Accepted connection from %s: %s\n" host (show port)
        forkFinally (talk handle) (\_ -> hClose handle)
