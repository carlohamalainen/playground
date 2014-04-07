{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Tinkering with the types and DSL features of
-- https://github.com/lucasdicioccio/laborantin-hs

-- This is NOT a replacement for Laboratin in any way.

module MiniLab where

import Data.Dynamic
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))

-- Dynamic environment map.
type DynEnv = M.Map Text Dynamic

-- An empty dynamic environment.
emptyEnv :: DynEnv
emptyEnv = M.empty

-- An execution error, just carries a string or a default message.
data ExecutionError = ExecutionError String deriving Show

instance Error ExecutionError where
  noMsg    = ExecutionError "A String Error!"
  strMsg   = ExecutionError

-- A parameter is just a string, integer pair.
data Parameter = MkParameter Text Integer deriving Show

-- An execution may be running, or finished running and successful or
-- finished running but with an error.
data ExecutionStatus = Running | Success | Failure deriving (Show, Read, Eq)

-- An Execution represents an ongoing (executing now) or past experiment result.
data Execution = MkExecution {
    eParameterSet       :: [Parameter]
  , eStatus             :: ExecutionStatus
  , eScenario           :: ScenarioDescription
} deriving Show

-- FIXME only for testing...
-- emptyExecution :: Execution
-- emptyExecution = MkExecution [] Failure emptyScenario

-- We need a reader for the back end and execution, neither of which we can modify.
-- This type is parameterised over the monad m.
type Foo1 m = ReaderT (Backend m, Execution) m

-- Next we wrap Foo1 with a state to hole the dynamic environment.
type Foo2 m = StateT DynEnv (Foo1 m)

-- Finally we add errors. We have our type ExecutionError (basically a string),
-- the underlying monad Foo2 m, and the result type a.
type Foo3 m a = ErrorT ExecutionError (Foo2 m) a

type Step m a = Foo3 m a -- synonym to line up with the name in Laboratin.

-- A backend stores all the functions that we need to run an experiment.
-- An alternative is to make this a typeclass.
data Backend m = MkBackend {
    bName :: Text

  -- the function bRun takes an execution, which specifies a particular
  -- set of parameters in a certain scenario, and attempts to run it.
  , bRun  :: Execution -> Step m ()
  -- , bRun :: m () -- FIXME

  -- FIXME
  , bPrepareExecution :: ScenarioDescription -> [Parameter] -> m Execution
}

-- A scenario carries all info needed to run an experiment.
data ScenarioDescription = MkScenarioDescription {
    sName   :: Text
  , sDesc   :: Text
  , sParams :: [[Parameter]] -- List of parameter sets to use
} deriving Show

emptyScenario :: ScenarioDescription
emptyScenario = MkScenarioDescription "" "" []




-- Execute a scenario with a given set of parameters.
executeScenario :: (Functor m, Monad m) => Backend m -> ScenarioDescription -> [Parameter] -> m Execution
executeScenario (b :: Backend m) sc prm = do
    -- Get the execution out of the backend, along with the supplied scenario
    -- and particular parameter set (a list of Parameter values).
    exec <- bPrepareExecution b sc prm  :: m Execution

    -- Extract the step from the execution - this looks like a trivial function
    -- but in laboratin, the blap function does a bit more including
    -- setup, run, teardown, etc, so it is more general.
    let step = blap exec                :: Step m ()

    -- To run the step we need to use the error, state, and reader transformer
    -- run functions.

    let run01 = runErrorT step              :: Foo2 m (Either ExecutionError ())
        run02 = runStateT run01 emptyEnv    :: Foo1 m (Either ExecutionError (), DynEnv)
        run03 = runReaderT run02 (b, exec)  :: m (Either ExecutionError (), DynEnv)

    -- Now we can actually run the action.
    status <- fst <$> run03 -- :: m (Either ExecutionError (), DynEnv)

    case status of Left _   -> return $ exec { eStatus = Failure }
                   Right () -> return $ exec { eStatus = Success }

    where

    -- In Laboratin this function does a bit more, namely
    -- the setup, run, teardown, and analyze steps. Here we have
    -- a single run function to do.
    blap :: Execution -> Step m ()
    blap exec = bRun b exec

exampleBackend :: Backend IO
exampleBackend = MkBackend "backend" exampleRun examplePrepare
    where

    exampleRun :: Execution -> Step IO ()
    exampleRun (MkExecution params status scenarioDesc) = do liftIO $ print $ "I am the run function: " ++ show (params, status, scenarioDesc)

    examplePrepare :: ScenarioDescription -> [Parameter] -> IO Execution
    examplePrepare sd params = return $ MkExecution params Failure sd -- Start off in failure state? I guess this is reasonable.

exampleTopLevelParameters :: [[Parameter]]
exampleTopLevelParameters = [ [MkParameter "foo" 1]
                            , [MkParameter "foo" 2]
                            ]

exampleScenario :: ScenarioDescription
exampleScenario = MkScenarioDescription "scenario" "blah blah blah" exampleTopLevelParameters


-- Now the DSL type functions.

-- DSL Entry point to build a scenario with default options.
scenario :: Text -> State ScenarioDescription () -> ScenarioDescription
scenario name f = execState f sc0
    where sc0 = emptyScenario { sName = name }

-- Add description to a scenario.
describe :: Text -> State ScenarioDescription ()
describe d = modify $ \sd -> sd { sDesc = d }

-- DSL entry point for creating a parameter.
parameter :: Text -> State Parameter () -> State ScenarioDescription ()
parameter p f = do
    MkScenarioDescription name desc params <- get
    let newParam = execState f (MkParameter p 0)

    let otherParams = if null params then [] else tail params        :: [[Parameter]]
    let newHead = newParam:(if null params then [] else head params) :: [Parameter]

    put $ MkScenarioDescription name desc (newHead:otherParams)

-- Setting the value on a parameter.
value :: Integer -> State Parameter ()
value x = do (MkParameter p _) <- get
             put $ MkParameter p x

main :: IO ()
main = do e <- executeScenario exampleBackend exampleScenario (head exampleTopLevelParameters)
          print e

-- blerp :: State ScenarioDescription ()
blerp = do
    scenario "this scenario" $ do
        describe "description"
        parameter "param1" $ do
            value 1
        parameter "param2" $ do
            value 2


main' :: IO ()
main' = do e <- executeScenario exampleBackend blerp params
           putStrLn ""
           putStrLn "In main', the result e:"
           putStrLn ""
           putStrLn $ show e
    where sd = blerp
          params = head $ sParams sd



