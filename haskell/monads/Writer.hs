{-# LANGUAGE InstanceSigs, MultiParamTypeClasses,  FunctionalDependencies, FlexibleInstances #-}


module Writer where

import Data.Monoid

data Hole = Hole
hole = undefined

-- Writer wraps up a value and a result...

newtype Writer w a = MkWriter { runWriter :: (a, w) }

instance (Monoid w) => Monad (Writer w) where
    return :: a -> Writer w a
    return a = MkWriter (a, mempty)

    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    (MkWriter (a, w)) >>= f = MkWriter (b, w `mappend` w')
         where (b, w') = runWriter $ f a



class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    pass    :: m (a,w -> w) -> m a
    listen  :: m a -> m (a, w)
    tell    :: w -> m ()

instance (Monoid w) => MonadWriter w (Writer w) where
    pass (MkWriter ((a,f),w)) = MkWriter (a,f w)
    listen (MkWriter (a, w)) = MkWriter ((a, w), w)
    tell s = MkWriter ((), s)

censor :: (MonadWriter w m) => (w -> w) -> m a -> m a
censor f m = pass $ do a <- m
                       return (a, f)

logMessage :: String -> Writer [String] ()
logMessage s = tell [s]

blah' :: Writer [String] (Integer, [String])
blah' = listen $ do MkWriter ((), ["hey"])
                    logMessage "again"
                    return 999


blah'' :: Writer [String] Integer
blah'' = censor nuke $ do MkWriter ((), ["hey"])
                          logMessage "again"
                          return 999
    where nuke _ = []


