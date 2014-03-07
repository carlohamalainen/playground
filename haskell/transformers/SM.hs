{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, InstanceSigs, RankNTypes #-}

module SM where

import Control.Monad

import System.Random
            ( StdGen
            , Random
            , random
            , randomR
            , mkStdGen
            )

data Hole = Hole
hole = undefined

newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)

    -- x :: s -> (a, s)
    -- f :: a -> State s b
    (State x) >>= f = State $ runner
        where runner s = (runState (f b)) s'
                where (b, s') = x s


eg1 = do
    let s0 = return 3 :: State Bool Int
        s1 = s0 >>= (\n -> State $ \s -> (n + 1, not s))
        s2 = s1 >>= (\n -> return $ n + 100)

    print $ runState s0 False
    print $ runState s1 False
    print $ runState s2 False


class Monad m => MonadState m s | m -> s where
    get :: m s
    put :: s -> m ()

    -- See mtl package, Control.Monad.State.Lazy, which defines this function:
    -- Just embeds a state action inside MonadState.
    state :: (s -> (a, s)) -> m a
    state f = do
        s <- get
        let ~(a, s') = f s -- Not sure why we need a lazy match, see http://en.wikibooks.org/wiki/Haskell/Laziness
        put s'
        return a

instance MonadState (State s) s where
    get   = State $ \s -> (s, s) -- get state by copying it as the value
    put s = State $ \_ -> ((), s) -- set the state, don't yield any value


getAny :: (Random a) => State StdGen a
getAny = do g <- get
            (x, g') <- return $ random g
            put g'
            return x

eg2 = do
    let x = getAny :: State StdGen Int
    print $ fst $ runState x (mkStdGen 1)

getOne :: (Random a) => (a, a) -> State StdGen a
getOne bounds = do
    g <- get
    (x, g') <- return $ randomR bounds g
    put g'
    return x

eg3 = do
    let x = getOne (1, 25) :: State StdGen Int
    print $ runState x (mkStdGen 1)

eg4 = do
    x <- getOne (1, 25) :: State StdGen Int
    return x

eg5 :: State StdGen (Int, Int, Char)
eg5 = do
    x <- getOne (1, 25)     :: State StdGen Int
    y <- getOne (10, 30)    :: State StdGen Int
    z <- getOne ('a', 'z')  :: State StdGen Char
    return (x, y, z)

eg6 :: StdGen -> ((Int, Int, Char), StdGen)
eg6 = runState eg5

eg7 = print $ eg6 $ mkStdGen 1


-- See Control.Monad.State.Lazy again...
modify :: (MonadState m s) => (s -> s) -> m ()
modify f = state (\s -> ((), f s))




