{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module TypeCheckId where

data Thud a = MkThud deriving (Show)

instance Monad Thud where
    return _ = MkThud
    _ >>= _  = MkThud

data Flip a = MkFlip Bool a deriving (Show)

data Compose f g x = MkCompose (f (g x))

instance Monad Flip where
    return :: a -> Flip a
    return = MkFlip False   -- or, return x = MkFlip False x

    (>>=) :: Flip a -> (a -> Flip b) -> Flip b
    MkFlip False x >>= f = f x
    MkFlip True  x >>= f = MkFlip (not b) y
        where MkFlip b y = f x

instance Monad (Compose Flip Thud) where
    return :: a -> Compose Flip Thud a
    return x = MkCompose (MkFlip True MkThud)

    (>>=) :: Compose Flip Thud a -> (a -> Compose Flip Thud b) -> Compose Flip Thud b
    (>>=) = undefined

expr = (MkCompose (MkFlip True MkThud)) >>= id

-- LHS of expr :: Compose Flip Thud (Compose Flip Thud b)
-- RHS of expr :: Compose Flip Thud b -> Compose Flip Thud b


