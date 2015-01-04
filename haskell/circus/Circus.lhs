<p> A coding-challenge kind of question, and a short foray into QuickCheck. Source is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/circus">https://github.com/carlohamalainen/playground/tree/master/haskell/circus</a>. </p>

<p> Here is a question from <a href="http://www.amazon.com/Cracking-Coding-Interview-Programming-Questions/dp/098478280X">Cracking the Coding Interview, 4th Edition</a>: </p>

<pre>
9.7 A circus is designing a tower routine consisting of people
standing atop one another's shoulders. For practical and aesthetic
reasons, each person must be both shorter and lighter than the person
below him or her. Given the heights and weights of each person in
the circus, write a method to compute the largest possible number of
people in such a tower.

EXAMPLE:

Input (ht, wt): (65, 100) (70, 150) (56, 90) (75, 190) (60, 95)
(68, 110)

Output: The longest tower is length 6 and includes from top to bottom:
(56, 90) (60,95) (65,100) (68,110) (70,150) (75,190)
</pre>

<p> The provided <a href="https://github.com/carlohamalainen/playground/blob/master/haskell/circus/JavaSoln.java">solution</a> is written in Java and seems far too verbose for such a simple problem. </p>

<p> For contrast, here's one way to solve it in Haskell: </p>

> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
>
> module Circus where
>
> import Control.Applicative
> import Data.Function (on)
> import Data.List (inits, maximumBy, sort)
> import Test.QuickCheck
> import Test.QuickCheck.Modifiers()

<p> Model a person by an algebraic data type. The compiler is smart
enough to work out the instances for <code>Eq</code>, <code>Ord</code>,
and <code>Show</code> for us. The <code>Ord</code> instance works as
if we had defined the person as a tuple of ints. </p>

> data Person = Person Int Int deriving (Eq, Ord, Show)

<p> A valid list of people must satisfy the property that each person is
strictly less than the person next in the list. We can check this by
pattern matching on the list. If there are two people <code>p1</code>,
<code>p2</code>, then the list is valid if <code>p1 < p2</code> and
the remaining list is also valid. Other cases (the empty list or a
list with a single person) are valid by definition. </p>

> validSeries :: [Person] -> Bool
> validSeries (p1:p2:px) = p1 < p2 && validSeries (p2:px)
> validSeries _          = True

<p> Checking this function: </p>

<pre>
*Circus> validSeries [Person 100 65, Person 150 80]
True

*Circus> validSeries [Person 100 65, Person 150 80, Person 140 70]
False
</pre>


<p> Given a <b>sorted</b> list of people, we are interested in the
longest valid sequence from the start of the list.
The <code>inits</code> function provides all initial lists:

<pre>
*Circus> inits [Person 100 65, Person 150 80, Person 140 70]
[
  [],
  [Person 100 65],
  [Person 100 65, Person 150 80],
  [Person 100 65, Person 150 80, Person 140 70]
]
</pre>

<p> So we want to keep taking elements from <code>inits</code> of
our list of people, while the list is a valid series. The
<code>takeWhile</code> function does, taking a boolean predicate as its first argument. Finally, we want the longest
such list, so we take the <code>last</code> element which is the longest one. </p>

> longestInit :: [Person] -> [Person]
> longestInit ps = last $ takeWhile validSeries $ inits ps

<p> To me this is easier to read than the rough equivalent in Java,
which involves some gnarly loops and a return value that encodes
detail by returning the value of <code>startFrom</code> when the loop
doesn't have enough items to consume: </p>

<pre>
int fillNextSeq(int startFrom, ArrayList<HtWt> seq) {
    int firstUnfitItem = startFrom;
    if (startFrom < items.size()) {
        for (int i = 0; i < items.size(); i++) {
            HtWt item = items.get(i);
            if (i == 0 || items.get(i-1).isBefore(item)) {
                seq.add(item);
            } else {
                firstUnfitItem = i;
            }
        }
    }
    return firstUnfitItem;
}
</pre>

<p> Not my cup of tea! </p>

<p> Finally, to find the solution we find the longest sequence by
sorting the input and looking for the longest sequence from the
initial sequence and those in any later lists: </p>

> soln0 :: [Person] -> [Person]
> soln0 px = maximumBy (compare `on` length) (findLongestSeq (sort px))
>
> findLongestSeq :: [Person] -> [[Person]]
> findLongestSeq [] = []
> findLongestSeq ps = initSeq:(findLongestSeq rest)
>   where
>     initSeq = longestInit ps
>     rest    = drop (length initSeq) ps

<p> Let's try it out on the question's sample data: </p>

> sampleInput :: [Person]
> sampleInput = [ Person 65 100
>               , Person 70 150
>               , Person 56 90
>               , Person 75 190
>               , Person 60 95
>               , Person 68 110
>               ]

<pre>
*Circus> soln0 sampleInput
[Person 56 90,Person 60 95,Person 65 100,Person 68 110,Person 70 150,Person 75 190]

*Circus> length $ soln0 sampleInput
6
</pre>

<p> But <code>soln0</code> has a bug - an unhandled case. Can you spot it? </p>

<p> Let's write an instance for <code>Arbitrary</code> so that we can use <a href="https://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2">QuickCheck</a>.  </p>

> instance Arbitrary Person where
>     arbitrary = Person <$> pos1 <*> pos2
>         where pos1 = getPositive <$> arbitrary
>               pos2 = getPositive <$> arbitrary

<p> We have used <code>getPositive</code> from <a href="https://hackage.haskell.org/package/QuickCheck-2.7.3/docs/Test-QuickCheck-Modifiers.html">Test.QuickCheck.Modifiers</a> because a person cannot have negative height or weight. </p>

<p> One property that we can check is that the solution does not change even if we reverse the input list. (We'd expect this to hold because we sort the list before looking for longest strictly increasing sequences.) Here is a function to check this property: </p>

> qc_soln0_1 = quickCheck (\pl -> soln0 pl == soln0 (reverse pl))

<p> It blows up, and QuickCheck provides us with the smallest input that causes a problem: </p>

<pre>
*Circus> qc_soln0_1
*** Failed! Exception: 'List.maximumBy: empty list' (after 1 test):
[]
</pre>

<p> In this case, it's the empty list being supplied
to <code>maximumBy</code>. Indeed, <code>findLongestSeq</code> on an empty list is just the empty list: </p>

<pre>
*Circus> findLongestSeq []
[]

*Circus> maximumBy (compare `on` length) []
*** Exception: List.maximumBy: empty list
</pre>

<p> Perhaps we should have used a <a href="https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell">Safe</a> variant of <code>maximumBy</code>. In any case, we have to handle the situation where the result is an empty list: </p>

> soln :: [Person] -> [Person]
> soln px = if result == [] then [] else maximumBy (compare `on` length) result
>   where
>     result = findLongestSeq $ sort px

<p> This definition appears to be correct, and passes all of the following tests: </p>

> -- No change if we reverse the input list:
> qc_soln_1 = quickCheck (\pl -> soln pl == soln (reverse pl))
>
> -- Computing the solution of the solution is identical to the solution:
> qc_soln_2 = quickCheck (\pl -> soln pl == (soln . soln) pl)
>
> -- Computing the solution of the solution of the solution is idential to the original solution:
> qc_soln_3 = quickCheck (\pl -> soln pl == (soln . soln . soln) pl)
>
> -- The length of the solution is not longer than the input:
> qc_soln_4 = quickCheck (\pl -> length (soln pl) <= length pl)
>
> -- A solution can be extended in a valid way, and solving on that input gives an
> -- answer of length (original solution) + 1:
> qc_soln_5 = quickCheck (\pl -> let pl' = extendByOne (soln pl) in length pl' == length (soln pl) + 1)
>   where
>     extendByOne :: [Person] -> [Person]
>     extendByOne [] = [Person 1 1]
>     extendByOne ps = ps ++ [Person (h + 1) (w + 1)]
>       where
>         Person h w = last ps
>
> -- Extending the input with a duplicate value gives a solution that is never longer
> -- than the original solution:
> qc_soln_6 = quickCheck (\pl -> length (soln pl) >= length (soln $ extendWithDup pl))
>   where
>     extendWithDup :: [Person] -> [Person]
>     extendWithDup [] = []
>     extendWithDup ps = ps ++ [last ps]

<p> Verifying these properties at the repl: </p>

<pre>
*Circus> qc_soln_1
+++ OK, passed 100 tests.

*Circus> qc_soln_2
+++ OK, passed 100 tests.

*Circus> qc_soln_3
+++ OK, passed 100 tests.

*Circus> qc_soln_4
+++ OK, passed 100 tests.

*Circus> qc_soln_5
+++ OK, passed 100 tests.

*Circus> qc_soln_6
+++ OK, passed 100 tests.
</pre>

<p> This is a fairly basic use of QuickCheck. For more detail about its background and some examples from the Erlang version of the library, check out this talk by John Hughes: </p>


<p> <center> <iframe width="560" height="315" src="//www.youtube.com/embed/XgasxJWgZBM?list=PLNFZ6HfKMrG1tz5aiYNXlu-k0b_qC2Ei4" frameborder="0" allowfullscreen></iframe> </center> </p>


