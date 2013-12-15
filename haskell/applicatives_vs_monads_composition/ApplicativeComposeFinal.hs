{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}

module ApplicativeCompose where

import Data.Maybe()
import Control.Applicative
import Control.Monad()

data Hole = Hole
hole = undefined

data Compose f g x = MkCompose (f (g x)) deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (MkCompose z) = MkCompose (fmap (fmap f) z)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: forall a. a -> Compose f g a
    pure x = MkCompose (pure (pure x))

    (<*>) :: forall a b.  Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (MkCompose f) <*> (MkCompose x) = MkCompose $ (convert f) <*> x
        where _ = f :: f (g (a -> b))
              _ = x :: f (g a)

              convert :: f (g (a -> b)) -> f (g a -> g b)
              convert fs = (pure (<*>)) <*> fs
