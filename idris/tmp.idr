import Data.SortedMap

-- try out templating...

-- types as values, so make a type that represents an option, an exclusive set of options?

-- type provider, parse spec?... maybe not.



-- Simple option: arg string, description.

data SimpleOpt : Type -> Type -> Type where
  MkSimpleOpt : a -> String -> SimpleOpt a String

opt1 : SimpleOpt Int String
opt1 = MkSimpleOpt 10 "--option1"

opt2 : SimpleOpt Int String
opt2 = MkSimpleOpt 20 "--option2"

opt3 : SimpleOpt Int String
opt3 = MkSimpleOpt 30 "--option3"

-- For the moment, just use equality on the arg string to determine
-- it two options are the same.
instance (Eq a) => Eq (SimpleOpt a String) where
  (MkSimpleOpt value argstr) == (MkSimpleOpt value' argstr') = argstr == argstr'

-- Now encode the requirement that opt1 and opt2 can't be
-- used at the same time.

-- Start inductively?

-- data OptionVector : 

-- (n : Nat) -> Vector n (SimpleOpt Bool String) -> Vector n (SimpleOpt Bool String)


renderSimpleOpt : Show a => SimpleOpt a String -> String
renderSimpleOpt (MkSimpleOpt value argstr) = argstr ++ " " ++ show value

eg1 : IO ()
eg1 = do
    putStrLn $ renderSimpleOpt opt1



data Exclusive : Bool -> Type -> Type where
  ExclusiveUnset : Exclusive False a
  ExclusiveAdd : a -> Exclusive False a -> Exclusive True a

ex0 : Exclusive False Int
ex0 = ExclusiveUnset

ex1 : Exclusive True Int
ex1 = ExclusiveAdd 1 ex0

-- Will be a type error:
-- ex2 : Exclusive True Int
-- ex2 = ExclusiveAdd 2 ex1




data Blah : (SortedMap Int Bool) -> Type -> Type where
  BlahInit : (x : SortedMap Int Bool) -> a -> Blah x a
  BlahSet  : (n : Int) -> Blah (insert n True empty) a -> Blah empty a

z0 : Blah empty String
z0 = BlahInit empty "foo"

z1 : Blah (insert 1 True $ empty) String
z1 = BlahInit (insert 1 True $ empty) "foo"

z2 : Blah (insert 2 True $ empty) String
z2 = BlahInit (insert 2 True $ empty) "foo"

-- hmm1 : Blah Empty String
-- hmm1 = BlahSet 1 z2

hmm2 : Blah Empty String
hmm2 = BlahSet 2 z2





