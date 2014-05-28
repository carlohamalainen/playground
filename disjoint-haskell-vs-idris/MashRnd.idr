
-- Compile this file using: idris MashRnd.idr -o MashRnd -p effects

module Main

import Effect.Random
import Effect.StdIO

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
  MkValidOptionList : (opts : List Option) -- WrappedOptionList opts
                   -> (xors : List (List Option))
                   -> {default Ok prf : IsNotClashing (notclash opts xors)}
                   -> ValidOptionList opts

runProgram : String -> ValidOptionList opts -> String
runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show opts

-- This type checks:
okProgram : String
okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)

-- This won't type check:
-- okProgram' : String
-- okProgram' = runProgram "/usr/local/prog" (MkValidOptionList myOptions23 myXors23)

{-
             Can't unify
                     IsNotClashing NotClashing
             with
                     IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2) (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (OptionsInIdris.notclash, intersect myOptions23 myXors23 myOptions23 y)) 2)))) id id myXors23 False) (Delay Clashing) (Delay NotClashing))

             Specifically:
                     Can't unify
                             NotClashing
                     with
                             Clashing
-}

%default partial

blap : { [RND] } Eff IO String
blap = do
  x <- rndInt 0 1

  let optsRnd = the (List Option) (if x == 0 then [opt2, opt3] else [opt2])

  -- This is ok:
  -- return $ runProgram "/usr/local/prog" (MkValidOptionList myOptions12  myXors23)

  -- but this is not ok, error is below:
  return $ runProgram "/usr/local/prog" (MkValidOptionList optsRnd myXors23)

main : IO ()
main = do x <- run blap
          return ()



{-

MashRnd.idr:99:60:When elaborating right hand side of blap:
When elaborating argument prf to constructor Main.MkValidOptionList:
        Can't unify
                IsNotClashing NotClashing
        with
                IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x1 => \y => x1 || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (Main.notclash, intersect optsRnd myXors23 optsRnd y)) 2) (length (Main.notclash, intersect optsRnd myXors23 optsRnd y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (Main.notclash, intersect optsRnd myXors23 optsRnd y)) 2)))) id id myXors23 False) (Delay Clashing) (Delay NotClashing))

        Specifically:
                Can't unify
                        NotClashing
                with
                        boolElim (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (Main.notclash, intersect (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [[(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]] (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)])) 2) (length (Main.notclash, intersect (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [[(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]] (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)])) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (Main.notclash, intersect (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [[(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]] (boolElim (intToBool (prim__eqBigInt x 0)) (Delay [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)]) (Delay [(MkOption "Do bar." "-bar" 99)])) [(MkOption "Do bar." "-bar" 99), (MkOption "Blah." "-blah" 0)])) 2)) (Delay Clashing) (Delay NotClashing)

-}
