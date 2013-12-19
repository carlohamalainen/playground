-- Muddle.hs

module Muddle where

import Data.Maybe

f :: a -> Maybe a
f x = Just x

s = "boo" :: String
t = Just 100 :: Maybe Int
main = print "Hello, World!"
