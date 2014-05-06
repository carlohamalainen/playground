<p> Inspiration for this blog post came from my work on <a href="https://github.com/carlohamalainen/volgenmodel-nipype">volgenmodel-nipype</a>,
a neuroimaging workflow written using <a href="http://nipy.sourceforge.net/nipype/">Nipype</a>. The Nipype
package allows one to wrap legacy command line applications using
traits to model the various command line options. Here's a snippet for <code>mincextract</code>
taken from <a href="https://github.com/carlohamalainen/volgenmodel-nipype/blob/master/nipypeminc.py">nipypeminc.py</a>: </p>

<pre>
class ExtractInputSpec(StdOutCommandLineInputSpec):
    input_file = File(
                    desc='input file',
                    exists=True,
                    mandatory=True,
                    argstr='%s',
                    position=-2,)

    output_file = File(
                    desc='output file',
                    position=-1)

    _xor_write = ('write_ascii', 'write_byte',
                  'write_short', 'write_int', 'write_long',
                  'write_float', 'write_double', 'write_signed',
                  'write_unsigned',)

    write_ascii = traits.Bool(
                desc='Write out data as ascii strings (default).',
                argstr='-ascii',
                xor=_xor_write)

    write_byte = traits.Bool(
                desc='Write out data as bytes.',
                argstr='-byte',
                xor=_xor_write)

    # snipped...
</pre>

<p> Note the xor condition, which means that the user cannot set <code>write_ascii</code> and <code>write_byte</code> at the same time. So this would be ok: </p>

<pre>
prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True)
</pre>

<p> but this would be rejected: </p>

<pre>
prog = Extract(input_file='/tmp/foo.mnc', write_ascii=True, write_byte=True)
</pre>

<p> A few times I made mistakes with these xor specifications
which resulted in run time errors. Recently I learned that
Haskell's type system can be used to make certain errors <a href="/blog/2013/11/13/tic-tac-toe-and-haskell-type-classes">compile time errors</a>
instead of run-time errors, and I naturally wondered
if an xor-type of condition could be encoded in Haskell's type system,
and if not that, perhaps in Idris. </p>

<p> A basic implementation in Haskell of a command line wrapper might look like this: </p>

> module OptionsInHaskell where

> import Control.Monad (when)
> import Data.List (intersect)

<p> A command line option has a description, argument string, and a
value. For simplicity we will only consider integer values. </p>

> data Option = MkOption { optDesc :: String
>                        , optArgStr :: String
>                        , optValue  :: Int
>                        } deriving Show

<p> Again, for simplicity, two options are considered equal if their
arg strings match: </p>

> instance Eq Option where
>   (MkOption _ a _) == (MkOption _ a' _) = a == a'

<p> Here are some example options: </p>

> opt1 :: Option
> opt1 = MkOption "Value of Foo."   "-foo"  34
>
> opt2 :: Option
> opt2 = MkOption "Do bar."         "-bar"  99
>
> opt3 :: Option
> opt3 = MkOption "Blah."           "-blah" 0

<p> A program consists of a path to a binary, a list of options, and a
list of xor conditions, which are lists of options that cannot be set simultaneously. </p>

> data Program = Program { progPath :: FilePath
>                        , progOptions :: [Option]
>                        , progXorOptions :: [[Option]]
>                        } deriving Show

<p> A list of options has a clash if it intersects with any of the xor-lists in
more than two elements: </p>

> clash :: [Option] -> [[Option]] -> Bool
> clash opts xors = any (\x -> length (intersect opts x) >= 2) xors

<p> We won't bother with the full details of spawning a process,
tidying up output files, capturing stdout and stderr, and so on,
so this <code>runProgram</code> function just prints some details
and checks that the options list is acceptable: </p>

> runProgram :: Program -> IO ()
> runProgram (Program path opts xorOpts) = do
>   putStrLn $ "Pretending to run: " ++ path
>   putStrLn $ "with options: " ++ show opts
>   putStrLn $ "and xor lists: " ++ show xorOpts
>   when (clash opts xorOpts) $ error "eek, options clash :("

<p> Here's a program with no xor conditions; it runs ok: </p>

> prog1 :: Program
> prog1 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] []

<pre>
*OptionsInHaskell> runProgram prog1
Pretending to run: /usr/local/bin/foo
with options: [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
              , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
              , MkOption {optDesc = "Blah.", optArgStr = "-blah", optValue = 0}
              ]
and xor lists: []
</pre>

<p> On the other hand, this program is not valid since options 1, 2, and 3 are set,
but the xor list specifies that options 1 and 2 cannot be set at the same time: </p>

> prog2 :: Program
> prog2 = Program "/usr/local/bin/foo" [opt1, opt2, opt3] [[opt1, opt2]]

<pre>
*OptionsInHaskell> runProgram prog2
Pretending to run: /usr/local/bin/foo
with options: [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
              , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
              , MkOption {optDesc = "Blah.", optArgStr = "-blah", optValue = 0}
              ]
and xor lists: [ [ MkOption {optDesc = "Value of Foo.", optArgStr = "-foo", optValue = 34}
                 , MkOption {optDesc = "Do bar.", optArgStr = "-bar", optValue = 99}
                 ]
               ]
*** Exception: eek, options clash :(
</pre>

<p> I'm not sure if we can make this a compile time error in Haskell, so I'll turn instead to Idris, where can exploit dependent types and other nice things. </p>

<p> First define an option data type, the quality instance, and a few examples: </p>

< module Other
<
< %default total
<
< data Option = MkOption String String Int
<
< instance Show Option where
<   show (MkOption x y z) = "Option " ++ show x ++ " " ++ show y ++ " " ++ show z
<
< instance Eq Option where
<   (MkOption _ a _) == (MkOption _ a' _) = a == a'
<
< opt1 : Option
< opt1 = MkOption "Value of Foo." "-foo" 34
<
< opt2 : Option
< opt2 = MkOption "Do bar." "-bar" 99
<
< opt3 : Option
< opt3 = MkOption "Blah." "-blah" 0

<p> Next we need to encode <i>in the type system</i> the result of an option list clashing or not: </p>

< data ClashValue = Clashing | NotClashing

<p> Checking if an option list has a clash is basically the same as in Haskell except that we return
a <code>ClashValue</code> instead of a <code>Bool</code>: </p>

< notclash : List Option -> List (List Option) -> ClashValue
< notclash opts xors = if (any (\x => length (intersect opts x) >= 2) xors)
<                            then Clashing
<                            else NotClashing
<    where intersect : Eq a => List a -> List a -> List a
<          intersect [] _ = []
<          intersect (x :: xs) ys = if x `elem` ys then x :: intersect xs ys
<                                                  else intersect xs ys

<p> Next, the tricky bit. We create a data type <code>IsNotClashing</code> which has
only one constructor, called <code>Ok</code>, that produces a value <code>IsNotClashing NotClashing</code>.
<i>There is no way to produce the value <code>IsNotClashing Clashing</code></i>.

< data IsNotClashing : ClashValue -> Type where
<   Ok : IsNotClashing NotClashing

<p> I'm a bit hazy on the details of the next chunk of code, as I wasn't able to get Idris to accept my
default proof clause in the <code>MkValidOptionList</code> data constructor. We aren't really doing anything here apart from
introducing two type synonyms <code>OptionList</code> and <code>OptionListList</code>, and a wrapped version of each. </p>

< OptionList : Type
< OptionList = List Option
<
< OptionListList : Type
< OptionListList = List (List Option)
<
< data WrappedOptionList : OptionList -> Type where
<   MkWrappedOptionList : (x : OptionList) -> WrappedOptionList x
<
< data XorLists : OptionListList -> Type where
<   MkXorLists : (x : OptionListList) -> XorLists x

<p> Example values, used later: </p>

< opts123 : WrappedOptionList   [opt1, opt2, opt3]
< opts123 = MkWrappedOptionList [opt1, opt2, opt3]
<
< opts12 : WrappedOptionList   [opt1, opt2]
< opts12 = MkWrappedOptionList [opt1, opt2]
<
< opts13 : WrappedOptionList   [opt1, opt3]
< opts13 = MkWrappedOptionList [opt1, opt3]
<
< myOptions12 : WrappedOptionList   [opt1, opt2]
< myOptions12 = MkWrappedOptionList [opt1, opt2]
<
< myOptions23 : WrappedOptionList   [opt2, opt3]
< myOptions23 = MkWrappedOptionList [opt2, opt3]
<
< myXors23 : XorLists   [[opt2, opt3]]
< myXors23 = MkXorLists [[opt2, opt3]]

<p> The heart of the solution is the <code>ValidOptionList</code> data type. We take
an option list and an xor list and, if a proof can be constructed for the value <code>Ok</code>
using the expression <code>IsNotClashing (notclash opts xors)</code>, then we produce
the actual value <code>ValidOptionList opts</code>. Due to the definition of <code>Ok</code>, this condition means
that <code>notclash opts xors</code> must evaluate to <code>NotClashing</code>. Hopefully this makes it clear why the
data types <code>ClashValue</code> and <code>IsNotClashing</code> were needed. </p>

< data ValidOptionList : OptionList -> Type where
<   MkValidOptionList : {default Ok prf : IsNotClashing (notclash opts xors)}
<                    -> WrappedOptionList opts
<                    -> XorLists xors
<                    -> ValidOptionList opts

<p> Finally, the <code>runProgram</code> function takes a path to an executable and a valid list of options. <i>The fact that the list of options is valid
is encoded in the type system</i>. </p>

< runProgram : String -> ValidOptionList opts -> String
< runProgram binary (MkValidOptionList opts xorsHere) = "pretended to run the program with options: " ++ show (unwrap opts)
<   where unwrap : WrappedOptionList o -> OptionList
<         unwrap (MkWrappedOptionList o) = o

<p> This program has options 1 and 2 set with the xor condition saying that options 2 and 3 cannot be set at the same time, so it type checks: </p>

< okProgram : String
< okProgram = runProgram "/usr/local/prog" (MkValidOptionList myOptions12 myXors23)

<p> On the other hand, this program with options 2 and 3 set does not type check, as expected: </p>

< notOkProgram : String
< notOkProgram' = runProgram "/usr/local/prog" (MkValidOptionList myOptions23 myXors23)

<p> The first part of the error is a bit scary: </p>

<pre>
 `-- When elaborating right hand side of notOkProgram:
     When elaborating argument prf to constructor Other.MkValidOptionList:
             Can't unify
                     IsNotClashing NotClashing
             with
                     IsNotClashing (boolElim (foldrImpl (flip (.) . flip (\x => \y => x || Delay (Prelude.Classes.Nat instance of Prelude.Classes.Ord, method > (Nat instance of Prelude.Classes.Ord, method compare (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2) (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2 || Delay (Nat instance of Prelude.Classes.Eq, method == (length (Other.notclash, intersect [opt2, opt3] [[opt2, opt3]] [opt2, opt3] y)) 2)))) id id [[opt2, opt3]] False) (Delay Clashing) (Delay NotClashing))
</pre>

<p> but the second part has the goods: </p>

<pre>
             Specifically:
                     Can't unify
                             NotClashing
                     with
                             Clashing
</pre>

<p> So there we have it. Compile-time error checking in Idris of a disjointness condition in the options for wrapping a legacy command line program. </p>

<p> <b> Further reading: </b> </p>

<ul>

<li> I stole the idea of <code>Ok</code> from <a href="https://gist.github.com/david-christiansen/0ead542a7f8d2ac3f689">David Christiansen's talk on error reflection</a> at the Idris <a href="https://github.com/idris-lang/Idris-dev/wiki/Idris-Developers-Meeting,-April-May-2014">developer's meeting</a>. </li>

<li> <a href="http://www.youtube.com/watch?v=fVBck2Zngjo">Idris: Type safe printf</a>, screencast by <a href="https://twitter.com/puffnfresh">Brian McKenna</a>. A similar sort of problem: printf format strings can be incorrectly specified, resulting in run-time exceptions in Haskell. Here, Brian shows how to make a type-safe printf function in Idris. </li>

</ul>

<p> Literate source for this post: <a href="https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInHaskell.lhs">OptionsInHaskell.lhs</a>. </p>

<p> Idris source: <a href="https://github.com/carlohamalainen/playground/blob/master/disjoint-haskell-vs-idris/OptionsInIdris.idr">OptionsInIdris.idr</a>. </p>

