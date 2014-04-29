%default total

remove : Eq a => a -> List a -> List a
remove x [] = []
remove x (y :: ys) = if x == y then ys else remove x ys

using (x:a, y:a, xs:List a)
  data Elem : a -> List a -> Type where
    here : Elem x (x :: xs)
    there : Elem x xs -> Elem x (y :: xs)

data Blah : (List Int) -> Type where
  BlahInit : (x : List Int) -> Blah x
  BlahSet  : (n : Int) -> Elem n xlist -> Blah xlist -> Blah (remove n xlist)

listOf3 : List Int
listOf3 = [1, 2, 3]

listOf2 : List Int
listOf2 = [1, 3]

b0 : Blah listOf3
b0 = BlahInit listOf3

elem1 : Elem 1 listOf3
elem1 = here

elem2 : Elem 2 listOf3
elem2 = there $ here

z2 : Blah [1, 3]
z2 ?= BlahSet 2 elem2 b0

