{-# LANGUAGE TemplateHaskell #-}

module L where

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- http://www.haskellforall.com/2013/05/program-imperatively-using-haskell.html

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)


-- score :: Lens' Game Int
-- score = lens _score (\game v -> game { _score = v })

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point


initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }


strike :: StateT Game IO ()
strike = do
    lift $ putStrLn "*shrink*"

    boss . health -= 10


go = do
    newstate <- execStateT strike initialState

    print $ "new state: " ++ show newstate

    print $ newstate ^. boss . health

