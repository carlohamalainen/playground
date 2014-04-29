
data MyVect : Nat -> Type -> Type where
  MyNil : MyVect Z a
  (::)  : a -> MyVect k a -> MyVect (S k) a

poo : List a -> Type
poo [] = Int
poo (x :: xs) = Float


-- Type that stores a nat and something else.
data OptionSet : Nat -> Type -> Type where
  OptionSetBase : OptionSet n a

opt1 : OptionSet Z String
opt1 = OptionSetBase


x : MyVect Z Int
x = MyNil

y : OptionSet Z Int
y = OptionSetBase

data Blop : Nat -> Type -> Type where
  BlopNil : Blop Z a
  BlopCons  : a -> Blop k a -> Blop (S k) a


data Option = MkOption String

data Interface = MkInterface String (List Option)




mkCommandWrapper : Interface
mkCommandWrapper = MkInterface "hello.sh" []


