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

-- OptionList : Type
-- OptionList = List Option

-- OptionListList : Type
-- OptionListList = List (List Option)

-- data WrappedOptionList : OptionList -> Type where
--   MkWrappedOptionList : (x : OptionList) -> WrappedOptionList x

-- data XorLists : OptionListList -> Type where
--   MkXorLists : (x : OptionListList) -> XorLists x

opts123 : List Option
opts123 = [opt1, opt2, opt3]

opts12 : List Option
opts12 = [opt1, opt2]

opts13 : List Option
opts13 = [opt1, opt3]

myOptions12 : List Option
myOptions12 = [opt1, opt2]

myOptions23 : List Option
myOptions23 = [opt2, opt3]

myXors23 : List (List Option)
myXors23 = [[opt2, opt3]]

data ValidOptionList : List Option -> Type where
  MkValidOptionList : {default Ok prf : IsNotClashing (notclash opts xors)}
                   -> (opts : List Option) -- WrappedOptionList opts
                   -> (xors : List (List Option))
                   -> ValidOptionList opts

runProgram : String -> ValidOptionList opts -> String
runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show opts

-- This type checks:
okProgram : String
okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)


{-

Actually, it doesn't:



`-- When elaborating right hand side of okProgram:
     When elaborating argument prf to constructor OptionsInIdris.MkValidOptionList:
             Can't unify
                     IsNotClashing NotClashing
             with
                     IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2) (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2)))) id id xors False) (Delay Clashing) (Delay NotClashing))
             
             Specifically:
                     Can't unify
                             NotClashing
                     with
                             boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2) (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (OptionsInIdris.notclash, intersect opts xors opts y)) 2)))) id id xors False) (Delay Clashing) (Delay NotClashing)

-}
