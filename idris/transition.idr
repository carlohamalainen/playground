-- See also http://carlo-hamalainen.net/blog/2013/11/13/tic-tac-toe-and-haskell-type-classes

-- Three states:

data State0 = MkState0
data State1 = MkState1
data State2 = MkState2

-- The Transition type encodes the initial state and
-- the two permissible transitions:

data Transition : Type -> Type where
  initial : Transition State0
  move01 : Transition State0 -> Transition State1
  move12 : Transition State1 -> Transition State2


-- Apply a transition to the initial state:

ok0 : ?rhs0
ok0 = move01 initial

-- Will not typecheck, error is below:

-- ok1 : ?rhs1
-- ok1 = move12 initial

{-
Can't unify
        Transition State0
with
        Transition State1

Specifically:
        Can't unify
                State0
        with
                State1

-}
