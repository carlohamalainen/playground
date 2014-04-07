{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Identity where

-- The identity monad does nothing, we'd normally just do function application.
newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return = Identity
  x >>= f = f $ runIdentity x

-- derive the State monad using the StateT monad transformer
-- type State s a = StateT s Identity a

---------------------------------------------------------------


class Error a where
  noMsg :: a
  strMsg :: String -> a

class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
  throwError = Left
  catchError (Left e) handler = handler e
  catchError r _ = r

-- Adapted from the example on Control.Monad.Error's hackage page.
data LengthError = EmptyString
                 | TooLong Int
                 | OtherError String
                 deriving Show

instance Error LengthError where
  noMsg = OtherError "Derp, what happened?"
  strMsg = OtherError

-- Error Monad is an Either...
type LengthMonad = Either LengthError


calculate :: String -> LengthMonad Int
calculate [] = throwError EmptyString
calculate s | len > 5   = throwError $ TooLong len
            | otherwise = return len
    where len = length s

calc :: String -> LengthMonad Int
calc s = calculate s `catchError` Left


-----------------------

class Monad m => MonadPlus m where
   mzero :: m a
   mplus :: m a -> m a -> m a

instance MonadPlus [] where
   mzero = []
   mplus = (++)

data Hole = Hole
hole = undefined

-- See Control.Monad
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ a [] = return a
foldM f a (x:xs) = f a x >>= \fax -> foldM f fax xs

data Parsed = Parsed [Int] deriving Show

parse0 :: Parsed -> Char -> [Parsed]
parse0 (Parsed x) '0' = return $ Parsed $ x ++ [0]
parse0 _          _   = mzero

parse1 :: Parsed -> Char -> [Parsed]
parse1 (Parsed x) '0' = return $ Parsed $ x ++ [9]
parse1 (Parsed x) '1' = return $ Parsed $ x ++ [1]
parse1 _          _   = mzero

parse :: Parsed -> Char -> [Parsed]
parse p c = (parse0 p c) `mplus` (parse1 p c)

result = foldM parse (Parsed []) "001"
