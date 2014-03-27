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

{-

The tutorial says:

    We have picked Maybe as a computation context here; it needs to be a context which is available for
    every effect supported by eval. In particular, because we have exceptions, it needs to be a context which
    supports exceptions. Alternatively, Either String or IO would be fine, for example.

So change "Maybe Integer" to "Either String Integer" and write a new version of runEval:

-}

either_runEval : List (String, Integer) -> Expr -> (Either String Integer)
either_runEval args expr = run (eval' expr)
  where eval' : Expr -> { [EXCEPTION String, STATE ExprEnv] } Eff (Either String Integer)
        eval' e = do put args
                     eval e

{-

It doesn't work:

✗ 07:50:45 carlo@ubuntu13 {master} ~/playground/idris $ idris expressioncalculator.idr -p effects
     ____    __     _                                          
    /  _/___/ /____(_)____                                     
    / // __  / ___/ / ___/     Version 0.9.11.2-git:7862be6
  _/ // /_/ / /  / (__  )      http://www.idris-lang.org/      
 /___/\__,_/_/  /_/____/       Type :? for help                

Type checking ./expressioncalculator.idr
expressioncalculator.idr:56:15:When elaborating type of Main.either_runEval, eval':
Can't disambiguate name: Effects.Env.::, Prelude.List.::, Prelude.Stream.::, Prelude.Vect.::
Metavariables: Main.either_runEval

-}

-- either_runEvalExample : Either String Integer
-- either_runEvalExample = either_runEval [("x", 1)] (Add (Var "x") (Val 10))

-- either_runEvalExample' : Either String Integer
-- either_runEvalExample' = either_runEval [("x'", 1)] (Add (Var "x") (Val 10))
