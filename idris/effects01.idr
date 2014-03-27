-- Working through the Effects tutorial (14th March 2014 version).

module Main

import Effect.StdIO
import Effect.State
import Effect.Random
import Effect.Exception
import Effect.Select

hello : { [STDIO] } Eff IO ()
hello = do putStrLn "Hello world!"
           x <- getStr
           putStrLn $ "Hello " ++ x

main : IO ()
main = run hello


data BTree a = Leaf
             | Node (BTree a) a (BTree a)

instance Show a => Show (BTree a) where
  show Leaf = "Leaf"
  show (Node l x r) = "[" ++ (show l) ++ "] (" ++ (show x) ++ ") [" ++ (show r) ++ "]"

testTree : BTree String
testTree = Node (Node Leaf "Jim" Leaf)
                "Fred"
                (Node (Node Leaf "Alice" Leaf)
                "Sheila"
                (Node Leaf "Bob" Leaf))


treeTagAux : (i : Int) -> BTree a -> (Int, BTree (Int, a))
treeTagAux i Leaf = (i, Leaf)
treeTagAux i (Node l x r) = let (i', l') = treeTagAux i l in
                            let x' = (i', x) in
                            let (i'', r') = treeTagAux (i' + 1) r in
                                (i'', Node l' x' r')

treeTag : (i : Int) -> BTree a -> BTree (Int, a)
treeTag i x = snd $ treeTagAux i x

treeTagAux' : BTree a -> { [STATE Int] } Eff m (BTree (Int, a))
treeTagAux' (Node l x r)
    = do l' <- treeTagAux' l
         i  <- get
         put $ i + 1
         r' <- treeTagAux' r

         pure $ Node l' (i, x) r'

treeTag' : (i : Int) -> BTree a -> BTree (Int, a)
treeTag' i x = runPure $ do put i
                            treeTagAux' x

treeTag'' : (i : Int) -> BTree a -> BTree (Int, a)
treeTag'' i x = runPureInit [i] $ treeTagAux' x

treeTagAux2 : BTree a -> { [ 'Tag    ::: STATE Int,
                              'Leaves ::: STATE Int] } Eff m (BTree (Int, a))
treeTagAux2 Leaf = do n <- 'Leaves :- get
                      'Leaves :- put (n + 1)
                      -- Could have done
                      --    'Leaves :- update (+1)
                      pure Leaf

treeTagAux2 (Node l x r)
    = do l' <- treeTagAux2 l
         i  <- 'Tag :- get
         'Tag :- put (i + 1)
         r' <- treeTagAux2 r

         pure $ Node l' (i, x) r'

treeTag2 : (i : Int) -> BTree a -> (Int, BTree (Int, a))
treeTag2 i x = runPureInit ['Tag := i, 'Leaves := 0]
                    (do x' <- treeTagAux2 x
                        leaves <- 'Leaves :- get
                        pure (leaves, x'))
blah : IO ()
blah = print $ treeTag' 0 testTree

blah2 : IO ()
blah2 = print $ treeTag2 0 testTree

stateLength : { [STATE String] } Eff m Nat
stateLength = do x <- get
                 pure (length x)

stateLength' : { [STATE String] } Eff m Nat
stateLength' = pure (length !get)

hello' : { [STDIO] } Eff IO ()
hello' = do putStrLn "Hello world!"
            putStrLn $ "Hello " ++ !getStr

hello'' : { [STATE Int, STDIO] } Eff IO ()
hello'' = do putStr "Name? "
             putStrLn $ "Hello " ++ trim !getStr ++ "!"
             update (+1)
             putStrLn ("I've said hello to: " ++ show !get ++ " people")

runHello'' : IO ()
runHello'' = run hello''

randomNumber : { [RND, STDIO] } Eff IO ()
randomNumber = do srand 0
                  r <- rndInt 0 100
                  putStrLn $ "Random number: " ++ show r

triple : Int -> { [SELECT, EXCEPTION String] } Eff m (Int, Int, Int)
triple max = do z <- select [1..max]
                y <- select [1..z]
                x <- select [1..y]

                if (x*x + y*y == z*z)
                   then pure (x, y, z)
                   else raise "No triple."

runTriple : IO ()
runTriple = print $ run (triple 10)

-- In a Maybe context, triple will return the first triple
-- that it finds; with a List context it will return all
-- of the triples in the range.
--            
--     *effects01> :x runTriple'
--     Just (3, (4, 5))
--     [(3, (4, 5)), (6, (8, 10))]
--     MkIO (\w => prim_io_return ()) : IO ()

runTriple' : IO ()
runTriple' = do print $ the (Maybe _) $ run (triple 10)
                print $ the (List  _) $ run (triple 10)

vadd : Vect n Int -> Vect n Int -> Vect n Int
vadd [] [] = []
vadd (x::xs) (y::ys) = x + y :: vadd xs ys

-- TODO No idea about this decEq thing (from Decidable.Equality)
-- and also the Yes/No refl/contra.
vadd_check : Vect n Int -> Vect m Int ->
             { [EXCEPTION String] } Eff e (Vect m Int)
vadd_check {n} {m} xs ys with (decEq n m)
  vadd_check {n} {m=n} xs ys | (Yes refl)  = pure (vadd xs ys)
  vadd_check {n} {m}   xs ys | (No contra) = raise "Length mismatch"                                            
read_vec : { [STDIO] } Eff IO (p ** Vect p Int)
read_vec = do putStr "Number (-1 when done): "
              case run (parseNumber (trim !getStr)) of
                Nothing => do putStrLn "Input error"
                              read_vec
                Just v  => if (v /= -1)
                              then do (_ ** xs) <- read_vec
                                      pure (_ ** v :: xs)
                              else pure (_ ** [])
  where
    -- In the tutorial the more general type signature is given
    -- with "Eff m Int" but in its usage above it is really
    -- specialized to Eff Maybe Int...
    -- parseNumber : String -> { [EXCEPTION String] } Eff m Int
    parseNumber : String -> { [EXCEPTION String] } Eff Maybe Int
    parseNumber str
      = if all (\x => isDigit x || x == '-') (unpack str)
           then pure (cast str)
           else raise "Not a number"

{-
Todo why is it that when we run this in the REPL we don't see the
initial output:

    *effects01> :x run read_vec
    1
    2
    3
    -1
    Number (-1 when done): Number (-1 when done): Number (-1 when done): Number (-1 when done): MkIO (\w => prim_io_return (3 ** [1, 2, 3])) : IO (p ** Vect p Int)

Ditto for do_vadd, its output is not as I would expect.
-}


do_vadd : { [STDIO, EXCEPTION String] } Eff IO ()
do_vadd = do putStrLn "Vector 1"
             (_ ** xs) <- read_vec
             putStrLn "Vector 2"
             (_ ** ys) <- read_vec

             putStrLn $ show !(vadd_check xs ys)
