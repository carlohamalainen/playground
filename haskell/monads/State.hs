module State where

import Control.Monad.State


data Hole = Hole
hole = undefined

-- DSL for setting up a bunch of parameters.

data Parameter = Parameter String Int deriving Show

parameter :: String -> Int -> State [Parameter] ()
parameter s i = modify $ (++) [Parameter s i]

stuff :: State [Parameter] ()
stuff = do parameter "foo" 1
           parameter "bar" 2

main :: IO ()
main = print $ runState stuff []
