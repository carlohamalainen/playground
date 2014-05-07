module OptionsInIdris

%default total

data Option = MkOption String String Int

instance Show Option where
  show (MkOption x y z) = "Option " ++ show x ++ " " ++ show y ++ " " ++ show z

instance Eq Option where
  (MkOption _ a _) == (MkOption _ a' _) = a == a'

opt1 : Option
opt1 = MkOption "Value of Foo." "-foo" 34

opt2 : Option
opt2 = MkOption "Do bar." "-bar" 99

opt3 : Option
opt3 = MkOption "Blah." "-blah" 0

data ClashValue = Clashing | NotClashing

notclash : List Option -> List (List Option) -> ClashValue
notclash opts xors = if (any (\x => length (intersect opts x) >= 2) xors)
                           then Clashing
                           else NotClashing
   where intersect : Eq a => List a -> List a -> List a
         intersect [] _ = []
         intersect (x :: xs) ys = if x `elem` ys then x :: intersect xs ys
                                                 else intersect xs ys

data IsNotClashing : ClashValue -> Type where
  Ok : IsNotClashing NotClashing

OptionList : Type
OptionList = List Option

OptionListList : Type
OptionListList = List (List Option)

data WrappedOptionList : OptionList -> Type where
  MkWrappedOptionList : (x : OptionList) -> WrappedOptionList x

data XorLists : OptionListList -> Type where
  MkXorLists : (x : OptionListList) -> XorLists x

opts123 : WrappedOptionList [opt1, opt2, opt3]
opts123 = MkWrappedOptionList [opt1, opt2, opt3]

opts12 : WrappedOptionList [opt1, opt2]
opts12 = MkWrappedOptionList [opt1, opt2]

opts13 : WrappedOptionList [opt1, opt3]
opts13 = MkWrappedOptionList [opt1, opt3]

myOptions12 : WrappedOptionList [opt1, opt2]
myOptions12 = MkWrappedOptionList [opt1, opt2]

myOptions23 : WrappedOptionList [opt2, opt3]
myOptions23 = MkWrappedOptionList [opt2, opt3]

myXors23 : XorLists [[opt2, opt3]]
myXors23 = MkXorLists [[opt2, opt3]]

data ValidOptionList : OptionList -> Type where
  MkValidOptionList : {default Ok prf : IsNotClashing (notclash opts xors)}
                   -> (opts : List (List Option)) -- WrappedOptionList opts
                   -> XorLists xors
                   -> ValidOptionList opts

runProgram : String -> ValidOptionList opts -> String
runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show (unwrap opts)
  where unwrap : WrappedOptionList o -> OptionList
        unwrap (MkWrappedOptionList o) = o


-- This type checks:
okProgram : String
okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)

-- This won't type check:
-- okProgram' : String
-- okProgram' = runProgram "/usr/local/prog" (MkValidOptionList myOptions23 myXors23)

{-
 `-- When elaborating right hand side of okProgram':
     When elaborating argument prf to constructor Other.MkValidOptionList:
             Can't unify
                     IsNotClashing NotClashing
             with
                     IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2) (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2)))) id id [[opt2, opt3]] False) (Delay Clashing) (Delay NotClashing))
             
             Specifically:
                     Can't unify
                             NotClashing
                     with
                             Clashing
-}
