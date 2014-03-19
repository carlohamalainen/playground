{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Nested where

import qualified Data.Map as M
-- import Data.Time (UTCTime)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.Dynamic
import Data.Text (Text)
-- import qualified Data.Text as T
-- import Data.List (nub)

-- type EnvIO = IO

type DynEnv = [String] -- M.Map Text Dynamic
-- emptyEnv :: DynEnv
-- emptyEnv = M.empty

data ExecutionError = ExecutionError String
    deriving (Show)

instance Error ExecutionError where
  noMsg    = ExecutionError "A String Error!"
  strMsg   = ExecutionError

type Step m a = (ErrorT ExecutionError (StateT DynEnv (ReaderT (Backend m,Execution m) m)) a)

data ScenarioDescription m = SDesc {
    sName   :: Text
  , sDeps   :: [Dependency m]
  } deriving (Show)

s0 = SDesc "scenario zero" []

data ParameterDescription = PDesc {
    pName   :: Text
  , pValues :: [String]
  } deriving (Show,Eq,Ord)

sampleParameterDescription = PDesc "parameter name FOO" ["value1", "value2"]

data Execution m = Exec {
    eScenario :: ScenarioDescription m
  , ePath     :: FilePath
  , eAncestors   :: [Execution m]
} deriving (Show)

exec0 = Exec s0 "/tmp" []

data Dependency m = Dep {
      dName     :: Text
    , dCheck    :: Execution m -> m Bool
    }

instance Eq (Dependency m) where
    d1 == d2 = dName d1 == dName d2

instance Show (Dependency m) where
    show dep = "Dep {dName="
                ++ show (dName dep)
                ++ "}"

data Backend m = Backend {
    bName      :: Text
  , bRun       :: Execution m -> Step m ()
}


mystep :: Step IO Int
mystep = do
    (backend, execution) <- ask -- from the reader

    s <- get -- from the state

    modify $ \s -> s ++ ["new state list"]

    case s of [] -> throwError $ ExecutionError "boo, empty state :("
              s  -> liftIO $ print $ "hey, state is " ++ show s
    return 0

data Hole = Hole

-- (ErrorT ExecutionError (StateT DynEnv (ReaderT (Backend m,Execution m) m)) a)

-- go = runErrorT $ runStateT (runReaderT mystep initialReader) initialState
go = runReaderT (runStateT (runErrorT $ mystep) initialState) initialReader
    where initialState = ["initial state is bla lalala"]
          initialReader = (initialBackend, initialExecution)

          initialBackend = Backend "backend" (\e -> undefined)

          initialExecution = Exec s0 "/tmp" []


