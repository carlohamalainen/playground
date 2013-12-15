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
    pure x   = MkCompose (pure (pure x))

    (MkCompose fn) <*> (MkCompose x) = MkCompose $ ((pure (<*>)) <*> fn) <*> x 
