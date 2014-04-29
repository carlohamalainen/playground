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
          | Random Integer
            
ExprEnv : Type
ExprEnv = List (String, Integer)

EvalEff : Type -> Type
EvalEff t = { [STDIO, EXCEPTION String, RND, STATE ExprEnv] } Eff IO t

eval : Expr -> EvalEff Integer
eval (Val x) = return x
eval (Add x y) = do x' <- eval x
                    y' <- eval y
                    return $ x' + y'
eval (Var x) = do e <- get
                  case lookup x e of
                    Nothing  => raise $ "No such variable " ++ x
                    Just val => return val
eval (Random u) = do val <- rndInt 0 u
                     putStrLn $ show val
                     return val

runEval : List (String, Integer) -> Expr -> IO Integer
runEval args expr = run (eval' expr)
  where eval' : Expr -> EvalEff Integer
        eval' e = do put args
                     eval e
