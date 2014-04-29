module elem

import Decidable.Equality

using (xs : List a)
    data Elem  : a -> List a -> Type where
         Here  : Elem x (x :: xs)
         There : Elem x xs -> Elem x (y :: xs)

isElem : (x : Nat) -> (xs : List Nat) -> Maybe (Elem x xs)


