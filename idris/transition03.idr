%default total

data State : (n : Nat) -> Type where
  MkState : (n : Nat) -> State n

foo : State 0
foo = MkState 0

data Transition : (n : Nat) -> (m : Nat) -> (State n) -> (State m) -> Type where
  Initial   : Transition 0 0 (MkState 0) (MkState 0)
  Trans0to1 : Transition 0 1 (MkState 0) (MkState 1)
  Trans1to2 : Transition 1 2 (MkState 1) (MkState 2)

  -- 0 -> 1 -> 2 -> ...
  MkBase : Transition 0 1 (MkState 0) (MkState 1)
  MkNext : Transition n m (MkState n) (MkState m)
           -> Transition (S n) (S m) (MkState (S n)) (MkState (S m))
  
  Any : (n : Nat) -> (m : Nat) -> Transition n m (MkState n) (MkState m)

derp : (n : Nat) -> (m : Nat) -> Type
derp n m = Transition n m (MkState n) (MkState m)


data Move : (n : Nat) -> (m : Nat) -> (State n) -> (State m) -> Type where
  Start : (Initial : Transition 0 0 s s) -> Move 0 0 s s
  Step  : (Move n1 n2 s1 s2) -> (Transition n2 n3 s2 s3) -> (Move n1 n3 s1 s3)

-- Using Step as an infix operator.
ok0 : Move 0 2 (MkState 0) (MkState 2)
ok0 = (Start Initial `Step` Trans0to1) `Step` Trans1to2

ok1 : Move 0 2 (MkState 0) (MkState 2)
ok1 = (Start Initial `Step` (Any 0 1)) `Step` (Any 1 2)

ok2 : Move 0 2 (MkState 0) (MkState 2)
ok2 = (Start Initial `Step` MkBase) `Step` (MkNext MkBase)


