{-# LANGUAGE RankNTypes #-}

module B2 where

import Data.Maybe

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import qualified Data.Map as DM
import System.Directory
import System.FilePath.Posix
import System.IO
import System.Process
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String

s3Line :: forall u. ParsecT [Char] u Identity String
s3Line = do
    date <- many (noneOf " ")
    spaces

    time <- many (noneOf " ")
    spaces

    md5 <- many (noneOf " ")
    spaces

    path <- many (noneOf "\n")
    newline

    return date

stest :: forall u. ParsecT [Char] u Identity String
stest = do
    x <- many1 (noneOf " ")
    return x

f :: a -> Maybe a
f x = Just x

s = "boo" :: String

main = print "Hello, World!"
