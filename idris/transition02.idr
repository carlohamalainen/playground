-- Variation on an email from DRC received 2014-03-19.

-- States of the system. Concrete types.
data State = State0 | State1 | State2

-- Bloop constructor takes an int and a string;
-- we define two ways to construct Bloop: Bloop1 and Bloop2.
data Bloop : Int -> String -> Type where
  Bloop1 : Bloop 1 "one"
  Bloop2 : Bloop 2 "two"

  Bloop3 : (a : Int) -> Bloop a "parameterised over a"

-- Valid transitions in our system:
-- 0 -> 1, 1 -> 2
-- Initial state is State0.
data Transition : State -> State -> Type where
  Initial   : Transition State0 State0
  Trans0to1 : Transition State0 State1
  Trans1to2 : Transition State1 State2


data Move : State -> State -> Type where
  Step : Move s1 s2 -> Transition s2 s3 -> Move s1 s3
  Start : (Initial : Transition s s) -> Move s s

-- Using Step as an infix operator.
ok0 : Move State0 State2
ok0 = (Start Initial `Step` Trans0to1) `Step` Trans1to2

-- More verbosely:
ok0' : Move State0 State2
ok0' = Step y Trans1to2
       where x : Move State0 State0
             x = Start Initial

             y : Move State0 State1
             y = Step x Trans0to1
