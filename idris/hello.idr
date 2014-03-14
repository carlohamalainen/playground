
testIt : List Int
testIt = [1, 2, 3]

data MYNat = MyZero | MySucc MYNat -- my natural numbers

instance Show MYNat where
    show MyZero = "MyZero"
    show (MySucc k) = "MySucc " ++ show k

data MyVect : Nat -> Type -> Type where
    MyNil : MyVect Z a
    (::) : a -> MyVect k a -> MyVect (S k) a

plusVect : Vect n a -> Vect n a -> Vect n a
plusVect xs ys = ?plusVect_rhs

myVect0 : Vect Z Int
myVect0 = Nil

myVect1 : Vect (S Z) Int
myVect1 = [9]

myVect2 : Vect (S $ S Z) Int
myVect2 = [9, 8]

blahha : (n : Nat) -> Bool
blahha Z = True
blahha (S k) = blahha k


main : IO ();
main = do -- putStr "What is your name? "
          -- name <- getLine
          -- putStrLn ("Hello " ++ name)

          let x = MySucc $ MySucc $ MySucc MyZero

          putStrLn (show x)

          print $ index (fS $ fZ) myVect2






