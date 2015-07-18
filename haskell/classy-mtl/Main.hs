module Main where

import Control.Monad.Except
import Control.Monad.Reader

-- IO then Except then Reader:
type IoExceptReader = ReaderT Int (ExceptT String IO)

dostuff :: ReaderT Int (ExceptT String IO) Int
dostuff = do
    -- Can use ask here (ReaderT):
    r <- ask
    if r == 42
        -- Can also use throwError here (ExceptT)
        then throwError "Got 42, oh no"
        else return $ r + 1000

blah :: IoExceptReader Int
blah = local (+1) dostuff

-- IO then Reader then Error:
type IoReaderError = ExceptT String (ReaderT Int IO)

readerThing :: ReaderT Int IO Int
readerThing = do
    -- Can only use ask here, no ExceptT stuff, unlike dostuff.
    r <- ask
    return $ r + 1

blah' :: IoReaderError Int
blah' = thisLift $ local (+1) readerThing
  where
    -- Explicit type signature so we can see that here, lift is
    -- taking us from a ReaderT to an ExceptT with a ReaderT inside.
    thisLift :: ReaderT Int IO Int -> ExceptT String (ReaderT Int IO) Int
    thisLift = lift


main :: IO ()
main = do
    e <- runExceptT (runReaderT blah 100)

    case e of
        Left err    -> putStrLn $ "Error!? " ++ err
        Right x     -> putStrLn $ "Got value: " ++ show x

    e' <- runReaderT (runExceptT blah') 100

    case e' of
        Left err'   -> putStrLn $ "Error!? " ++ err'
        Right x'    -> putStrLn $ "Got value: " ++ show x'

    print ()
