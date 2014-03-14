module Tutorial01

foo : Int -> Int
foo x = case isLT of
            Yes => x*2
            No => x*4
   where
      data MyLT = Yes | No
      isLT : MyLT
      isLT = if x < 20 then Yes else No

even : Nat -> Bool
even Z = True
even (S k) = not $ even k

data MyVect : Nat -> Type -> Type where
  MyNil : MyVect Z a
  (::)  : a -> MyVect k a -> MyVect (S k) a

vect1 : MyVect 1 Int
vect1 = 1 :: MyNil

vect2 : MyVect 2 Int
vect2 = 2 :: vect1

myVectPP : MyVect n a -> MyVect m a -> MyVect (n + m) a
myVectPP MyNil x1 = x1
myVectPP (x :: y) x1 = x :: (myVectPP y x1)

myVectAdd : Num a => MyVect n a -> MyVect n a -> MyVect n a
myVectAdd MyNil MyNil = MyNil
myVectAdd (x :: y) (z :: w) = (x + z) :: (myVectAdd y w)

data Elem : a -> Vect n a -> Type where
  here : {x:a} -> {xs:Vect n a} -> Elem x (x :: xs)
  there : {x,y:a} -> {xs:Vect n a} -> Elem x xs -> Elem x (y :: xs)

testVec : Vect 4 Int
testVec = 3 :: 4 :: 5 :: 6 :: Nil

inVect5 : Elem 5 testVec
inVect5 = there $ there here

inVect3 : Elem 3 testVec
inVect3 = here

{-
This blows up (as expected):

inVect9 : Elem 9 testVec
inVect9 = here

  Can't unify
          Elem 3 [3, 4, 5, 6]
  with
          Elem 9 [3, 4, 5, 6]

  Specifically:
          Can't unify
                  3
          with
                  9

  In context:
          {a502} : Type
          {n503} : Nat
          {x504} : a
          {xs505} : Vect n a

-}

-- 'using' to avoid the implicit arguments
using (x:a, y:a, xs:Vect n a)
  data Blah : a -> Vect n a -> Type where
    blahHere  : Blah x (x :: xs)
    blahThere : Blah x xs -> Blah x (y :: xs)

blah3 : Blah 3 testVec
blah3 = blahHere

blah4 : Blah 4 testVec 
blah4 = blahThere $ blahHere


mutual
  even' : Nat -> Bool
  even' Z = True
  even' (S k) = odd' k

  odd' : Nat -> Bool
  odd' Z = False
  odd' (S k) = even' k


lam1 : Vect 4 Int
lam1 = map (\x => x + 1) testVec


{-

Currying:

*tutorial01> :t (*2)
\{ARG1000} => ARG * (fromInteger 2) : Integer -> Integer
*tutorial01> :t (2*)
\{ARG1000} => fromInteger 2 * ARG : Integer -> Integer

-}

testList : List Int
testList = [0, 1, 2, 3, 4]

myLookup : Nat -> List a -> Maybe a
myLookup k [] = Nothing
myLookup Z (x :: xs) = Just x
myLookup (S k) (x :: xs) = myLookup k xs


int3 : Int
int3 = 3

-- maybe3Plus1 : Maybe Int
-- maybe3Plus1 = maybe (Just int3) Nothing (+1)



-- Dependent pairs
depVec : (n : Nat ** Vect n Int)
depVec = (2 ** [3, 4])

-- Type checker infers the n:
depVec' : (n : Nat ** Vect n Int)
depVec' = (_ ** [3, 4])

-- Type checker infers the n and also its type:
depVec'' : (n ** Vect n Int)
depVec'' = (_ ** [3, 4])

filter' : (a -> Bool) -> Vect n a -> (p ** Vect p a)
filter' _ [] = (_ ** [])

-- The tutorial has this on one line, doesn't compile.
-- Also, what's this pipe (vertical bar) doing? Like a guard?
-- FIXME Answer is in 7.2, the 'with' lets us match on an intermediate
-- value, so (filter p xs) === (_ ** tail).
filter' p (x::xs) with (filter p xs)
    | (_ ** tail) =
          if p x then
              (_ ** x::tail)
          else
              (_ ** tail)



-- 3.8 "The so data type is a predicate on Bool..."
-- FIXME How do we actually run this?
blah : (x : Int) -> so (x >= 0) -> IO ()
blah x p = putStrLn $ show x


-- Interesting, can't do
--
--     data Person = Person String Int
--
-- as we can do in Haskell, makes the type and the constructor clearer.
data Person = MkPerson String Int

showPerson : Person -> String
showPerson p = let MkPerson n a = p in show n ++ " " ++ show a

goPerson : String
goPerson = showPerson $ MkPerson "Bob" 34


record Person' : Type where
  MkPerson' : (name : String) -> (age : Int) -> Person'

fred : Person'
fred = record { name = "Fred" } (MkPerson' "Bob" 34)


instance Eq Person' where
  (MkPerson' n1 a1) == (MkPerson' n2 a2) = n1 == n2 && a1 == a2

personEq1 : Bool
personEq1 = MkPerson' "Bob" 42 == MkPerson' "Bob" 42


madd : Num a => Maybe a -> Maybe a -> Maybe a
madd x y = do x' <- x
              y' <- y
              return $ x' + y'

-- Maybe is an instance of MonadPlus (mplus, mzero)...
madd' : Num a => Maybe a -> Maybe a -> Maybe a
madd' x y = [x' + y' | x' <- x, y' <- y]



appVal3 : Maybe Int
appVal3 = pure 3

appVal4 : Maybe Int
appVal4 = pure 4

-- idiom brackets, alternative to the <$> thing...
appValResult : Maybe Int
appValResult = [| appVal3 + appVal4 |]

-- The usual <$> stuff, as with Haskell's Applicative.
appValResult' : Maybe Int
appValResult' = pure (+) <$> appVal3 <$> appVal4


-- 4.4.1 error-handling interpreter

data Expr = Var String
          | Val Int
          | Add Expr Expr

-- list of variable to int bindings, can fail.
data Eval : Type -> Type where
  MkEval : (List (String, Int) -> Maybe a) -> Eval a


fetch : String -> Eval Int
fetch x = MkEval (\e => fetchVal e) where
  fetchVal : List (String, Int) -> Maybe Int
  fetchVal [] = Nothing
  fetchVal ((v, val) :: xs) = if x == v
                                 then Just val
                                 else fetchVal xs


-- FIXME Functor defines map, not fmap, in the current prelude/Prelude/Functor.idr
instance Functor Eval where
  map f (MkEval g) = MkEval (\e => map f (g e))

instance Applicative Eval where
  pure x = MkEval (\_ => Just x)

  (<$>) (MkEval f) (MkEval g) = MkEval (\x => app (f x) (g x)) where
    app : Maybe (a -> b) -> Maybe a -> Maybe b
    app (Just fx) (Just gx) = Just (fx gx)
    app _         _         = Nothing


-- eval now using idiomatic application brackets:
eval : Expr -> Eval Int
eval (Var x)    = fetch x
eval (Val x)    = [| x |]
eval (Add x y)  = [| eval x + eval y |]

-- Using applicative style:
eval' : Expr -> Eval Int
eval' (Var x)    = fetch x
eval' (Val x)    = pure x
eval' (Add x y)  = pure (+) <$> eval' x <$> eval' y


runEval : List (String, Int) -> Expr -> Maybe Int
runEval env e = envFn env
    where matchFn : Eval Int -> (List (String, Int) -> Maybe Int)
          matchFn (MkEval f) = f

          envFn : List (String, Int) -> Maybe Int
          envFn = matchFn $ eval e

-- The pattern matching on MkEval is painful, can use a case expression
-- to simplify this (as per the tutorial):
runEval' : List (String, Int) -> Expr -> Maybe Int
runEval' env e = case eval e of MkEval envFn => envFn env

egEval1 : Maybe Int
egEval1 = runEval [("x", 1)] (Var "x")


-- 4.5 Named instances, nice.

-- TODO: modules, namespaces, etc.

-- TODO 7.2, parity and natToBin.

-- TODO proofs by contradiction, FalseElim.


-- Equality is a congruence...
cong' : {f : t -> u} -> (a = b) -> f a = f b
cong' refl = refl

-- prove that plus Z n = n
plusReduces : (n:Nat) -> plus Z n = n
plusReduces n = refl

-- prove that n = plus n Z
-- plusReducesZ : (n:Nat) -> n = plus n Z
-- plusReducesZ Z = refl
-- plusReducesZ (S k) = ?plusReducesZ_rhs_2

-- doing it interactively...

plusReducesZ' : (n:Nat) -> n = plus Z n
plusReducesZ' Z = ?rhs_Z
plusReducesZ' (S k) = let ih = plusReducesZ' k in ?rhs_Z_S



---------- Proofs ----------

Tutorial01.rhs_Z_S = proof
  intros
  compute
  rewrite ih
  trivial 


Tutorial01.rhs_Z = proof
  refine refl


