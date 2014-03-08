{-# LANGUAGE DeriveDataTypeable #-}

module T where

import Data.Dynamic
import Data.Maybe

-- need DeriveDataTypeable language extension to do this:
data MyType a = MyType a deriving (Show, Eq, Typeable)


go = print $ typeOf (MyType (3.4::Float))

-- heterogeneous list
hlist :: [Dynamic]
hlist = [ toDyn (1::Int)
        , toDyn (2::Float)
        , toDyn ("boo"::String)
        ]

boo = do
    print hlist
    print $ head $ hlist

    let x0 = fromDynamic $ hlist !! 0 :: Maybe Int
    let x1 = fromDynamic $ hlist !! 1 :: Maybe Float
    let x2 = fromDynamic $ hlist !! 2 :: Maybe String
    print $ x0
    print $ x1
    print $ x2
