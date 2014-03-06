{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Ask where

import Control.Monad.Reader
import Control.Monad.State

inside0 :: ReaderT String IO Float
inside0 = do
    e <- ask :: ReaderT String IO String

    liftIO $ putStrLn $ "inside0, e: " ++ show e

    return 1.23

inside1 :: StateT [Int] (ReaderT String IO) Float
inside1 = do
    e <- ask :: StateT [Int] (ReaderT String IO) String
    s <- get :: StateT [Int] (ReaderT String IO) [Int]

    liftIO $ putStrLn $ "inside1, e: " ++ show e
    liftIO $ putStrLn $ "inside1, s: " ++ show s

    put [1, 1, 1]

    return 1.23

run0 :: IO ()
run0 = do let layer1 = runReaderT inside0 "reader environment, hi"

          result <- layer1

          print $ "result: " ++ show result

run1 :: IO ()
run1 = do let layer1 = runStateT inside1 [0]
              layer2 = runReaderT layer1 "reader environment, hi"

          (result, finalState) <- layer2

          print $ "final state: " ++ show finalState

          print $ "result: " ++ show result

inside1' :: StateT [Int] (ReaderT String IO) Float
inside1' = do
    e <- StateT $ \s -> do a <- ask
                           return (a, s)

    s <- get :: StateT [Int] (ReaderT String IO) [Int]

    liftIO $ putStrLn $ "inside1, e: " ++ show e
    liftIO $ putStrLn $ "inside1, s: " ++ show s

    put [1, 1, 1]

    return 1.23

run1' :: IO ()
run1' = do let layer1 = runStateT inside1' [0]
               layer2 = runReaderT layer1 "reader environment, hi"

           (result, finalState) <- layer2

           print $ "final state: " ++ show finalState

           print $ "result: " ++ show result
