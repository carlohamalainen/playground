-- Working through the Effects tutorial (14th March 2014 version).

module Main

import Effect.StdIO
import Effect.State
import Effect.Random
import Effect.Exception
import Effect.Select

data Expr = Val Integer
          | Add Expr Expr
          | Var String

ExprEnv : Type
ExprEnv = List (String, Integer)

eval : Expr -> { [EXCEPTION String, STATE ExprEnv] } Eff m Integer
eval (Val x) = return x
eval (Add x y) = do x' <- eval x
                    y' <- eval y
                    return $ x' + y'
eval (Var x) = do e <- get
                  case lookup x e of
                    Nothing  => raise $ "No such variable " ++ x
                    Just val => return val

runEval : List (String, Integer) -> Expr -> Maybe Integer
runEval args expr = run (eval' expr)
  where eval' : Expr -> { [EXCEPTION String, STATE ExprEnv] } Eff Maybe Integer
        eval' e = do put args
                     eval e

-- returns Just 11
runEvalExample : Maybe Integer
runEvalExample = runEval [("x", 1)] (Add (Var "x") (Val 10))

-- returns Nothing
runEvalExample' : Maybe Integer
runEvalExample' = runEval [("x'", 1)] (Add (Var "x") (Val 10))

io_runEval : List (String, Integer) -> Expr -> IO Integer
io_runEval args expr = run (eval' expr)
  where eval' : Expr -> { [EXCEPTION String, STATE ExprEnv] } Eff IO Integer
        eval' e = do put args
                     eval e

-- Note that we have (Either String) here to get the types right.
-- TODO: Push to the tutorial a short note on the bracketing,
-- initially I wrote
--     Eff (Either String) Integer
-- which produced strange error messages from Idris.                     
either_runEval : List (String, Integer) -> Expr -> (Either String) Integer
either_runEval args expr = run (eval' expr)
  where eval' : Expr -> { [EXCEPTION String, STATE ExprEnv] } Eff (Either String) Integer
        eval' e = do put args
                     eval e

-- Right 11 : Either String Integer
either_runEvalExample : Either String Integer
either_runEvalExample = either_runEval [("x", 1)] (Add (Var "x") (Val 10))

-- Left "No such variable x" : Either String Integer
either_runEvalExample' : Either String Integer
either_runEvalExample' = either_runEval [("x'", 1)] (Add (Var "x") (Val 10))
