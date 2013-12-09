{-# LANGUAGE InstanceSigs, RankNTypes, ScopedTypeVariables #-}

-- Carlo Hamalainen <carlo@carlo-hamalainen.net>

-- Looking back on an old post that I wrote: http://carlo-hamalainen.net/blog/2011/2/11/parsing-with-monads-can-be-slow

module Parser where

import Data.Char
import Data.Maybe
import System.Environment

data Hole = Hole
hole = undefined

-- A parser is a function that takes a String, and if successful
-- returns the remaining string and some kind of parsed object.
--
-- So the function would be String -> Maybe (String, a)
-- Use record syntax to name the 'parse' function which pulls
-- out the function from a (Parser a) value.

data Parser a = MkParser { parse :: String -> Maybe (String, a) }

-- but note that parse unwraps a Parser, so
-- parse :: Parser a -> (String -> Maybe (String, a))

-- parser that doesn't consume any input, just returns a value
-- in the parser context (this is 'return' in Monad-land):
value :: a -> Parser a
value x = MkParser $ \s -> Just (s, x)

-- Let's make Parser an instance of Monad. This will let us use
-- monadic 'do' notation.

instance Monad Parser where
    return = value

    (>>=) :: forall a b. Parser a -> (a -> Parser b) -> Parser b
    x >>= f = MkParser $ body
        where _ = x :: Parser a
              _ = f :: a -> Parser b
            
              body :: String -> Maybe (String, b)
              body s = case p s of Just (s', x') -> parse (f x') s'
                                   Nothing       -> Nothing
                where MkParser p = x

-- Parser that fails immediately
failed :: Parser a
failed = MkParser (\_ -> Nothing)

-- Parser that consumes a single character (any character)
character :: Parser Char
character = MkParser (\s -> if s == []
                                then Nothing
                                else Just (tail s, head s))

-- parser 1 or parser 2 if parser1 failed.
(|||) :: Parser a -> Parser a -> Parser a
(MkParser p1) ||| (MkParser p2) = MkParser $ \s -> case p1 s of Just (r, c) -> Just (r, c)
                                                                Nothing     -> p2 s

mapParser :: Parser a -> (a -> b) -> Parser b
-- mapParser (MkParser p) f = MkParser (\s -> case p s of Just (r, c) -> Just (r, f c)
--                                                        Nothing -> Nothing)
mapParser (MkParser (p :: String -> Maybe (String, a))) (f :: a -> b) = MkParser body
    where body :: String -> Maybe (String, b)
          body s = case p s of Just (r, c) -> Just (r, f c)
                               Nothing     -> Nothing

{-
Had some dramas with this one. Change the type signature to

    mapParser (MkParser (p :: String -> Maybe (String, a))) f = MkParser body

and it blows up with:

    Parser.hs:67:56:
        Couldn't match expected type `b1' with actual type `b'
          `b1' is a rigid type variable bound by
               the type signature for body :: String -> Maybe (String, b1)
               at Parser.hs:66:19
          `b' is a rigid type variable bound by
              the type signature for
                mapParser :: Parser a -> (a -> b) -> Parser b
              at Parser.hs:62:14
        In the return type of a call of `f'
        In the expression: f c
        In the first argument of `Just', namely `(r, f c)'
    Failed, modules loaded: none.

Something about ScopedTypeVariables that I do not understand - it
looks like GHC decides that

    f :: a -> b1

and then the b1 clashes with my explicitly named b.

Ditto for the first parameter, couldn't do (MkParser p) as this gave
an a1 type in the final tuple.

-}



-- Run p1, throw away the result, then run p2 and return its result.
(>>>) :: Parser a -> Parser b -> Parser b
p1 >>> p2 = do _ <- p1
               r <- p2
               return r

-- combine a list of parsers into a single parser that returns a list of results
-- easy to write with 'do' notation compared to using bind directly...
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = value []
sequenceParser (p:ps) = do r <- p
                           s <- sequenceParser ps
                           return (r:s)

-- list and many1 depend on each other. Nice.

-- 0 or more of a parser
list :: Parser a -> Parser [a]
list p = many1 p ||| value []

-- produce a parser than accepts one or more 
-- another nice one using do notation
many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p          -- run p once
    r <- list p     -- run p 0 or more times
    return (x:r)

-- parse a character only if it satisfies a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- character
               if p c then return c
                      else failed

is :: Char -> Parser Char
is c = satisfy (== c)

space :: Parser Char
space = satisfy isSpace

alpha :: Parser Char
alpha = satisfy isAlpha
