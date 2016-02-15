<p> This note about zippers follows <a href="https://www.lri.fr/~filliatr/publis/enum2.pdf">Backtracking Iterators (Jean-Christophe Filliâtre)</a>.</p>

> module Zipper where
>
> import Test.QuickCheck
> import Test.QuickCheck.Gen

<p> For our examples, we use a simple algebraic datatype, a balanced binary tree with integer labels for the nodes: </p>

> data Tree = Empty | Node Tree Int Tree
>           deriving (Eq, Show)

<p> Here is an example tree: </p>

> tree :: Tree
> tree = Node
>          (Node Empty 1 Empty)
>          2
>          (Node Empty 3 (Node Empty 4 Empty))

<p> We would normally draw this tree like this, with <code>E</code> for <code>Empty</code>: </p>

<pre>
      2
     / \
    /   \
   1     3
  / \   / \
 E   E E   4
          / \
         E   E
</pre>

<p> Think about traversing the tree. At the beginning there is no path
- we are at the top of the tree.  Otherwise we have either gone down
the left subtree or the right subtree. </p>

<p> For example, if we went down the left branch at a node,
we would have at hand the path that we followed to get to this node,
the value at the node (an integer), and the tree on the right subtree
that we did not visit. </p>

<p> Start at the top of the tree: </p>

<pre>
path: Top (haven't gone anywhere)

tree:
      2
     / \
    /   \
   1     3
  / \   / \
 E   E E   4
          / \
         E   E
</pre>

<p> Now walk down the left branch. </p>

<pre>
path: went left, have a 2, and the subtree
      to the right of us is
                                 3
                                / \
                               E   4
                                  / \
                                 E   E

we are focused on this subtree:

   1
  / \
 E   E

</pre>

<p> The type to represent this information: </p>

> data Path = Top                      -- ^ No path.
>           | WentLeft  Path Int Tree  -- ^ Followed the left subtree
>           | WentRight Tree Int Path  -- ^ Followed the right subtree
>           deriving (Eq, Show)

<p> A zipper is a tree and the path. </p>

> data Zipper = Zipper Tree Path
>             deriving (Eq, Show)

<p> The initial zipper is just the tree with no path. </p>

> create :: Tree -> Zipper
> create t = Zipper t Top

<p> Conversely, if we have a zipper and we are at the top,
we can get the tree out of it. </p>

> finish :: Zipper -> Tree
> finish (Zipper t Top) = t
> finish (Zipper t p)   = error $ "Can't finish here, path is " ++ show p ++ " with tree " ++ show t

<p> Intuitively, we would expect that <code>finish . create = id</code>, and we can check this
using <a href="https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html">QuickCheck</a>. First,
provide an <code>instance</code> of <code>Arbitrary</code> for our binary trees: </p>

> instance Arbitrary Tree where
>   arbitrary = frequency [ (1, return Empty) -- Empty
>                         , (1, arbNode)      -- Node <left> <n> <right>
>                         ]
>       where arbNode = do n <- arbitrary   -- arbitrary node label
>                          l <- arbitrary   -- arbitrary left subtree
>                          r <- arbitrary   -- arbitrary right subtree
>                          return $ Node l n r

<p> Now the property <code>finish . create = id</code> can be written as: </p>

> prop_finish_create t = finish (create t) == t

<p> Check it: </p>

<pre>
*Zipper> quickCheck prop_finish_create
+++ OK, passed 100 tests.
</pre>

<p> Looks good. Use <code>verboseCheck prop_finish_create</a> to see the values being generated. </p>

<p> Back to the zipper. Walking into the left subtree, as in the example above, involves
moving the focus to the left subtree, and noting the node and the
right subtree in the path component. </p>

> goDownLeft :: Zipper -> Zipper
> goDownLeft (Zipper Empty        _) = error "Can't go down-left on an empty tree."
> goDownLeft (Zipper (Node l x r) p) = Zipper l (WentLeft p x r)

<p> Going down the right subtree is similar: </p>

> goDownRight :: Zipper -> Zipper
> goDownRight (Zipper Empty        _) = error "Can't go down-right on an empty tree."
> goDownRight (Zipper (Node l x r) p) = Zipper r (WentRight l x p)

<p> Apply a function at a zipper, i.e. to the value with current focus: </p>

> setValue :: Zipper -> (Int -> Int) -> Zipper
> setValue (Zipper _ Top)               _ = error "Can't apply function with an empty context."
> setValue (Zipper t (WentLeft  p x r)) f = Zipper t (WentLeft  p (f x) r)
> setValue (Zipper t (WentRight l x p)) f = Zipper t (WentRight l (f x) p)

<p> For completeness we should be able to unroll a zipper. Zip it back up? </p>

> goUp :: Zipper -> Zipper
> goUp (Zipper l (WentLeft  p x r)) = Zipper (Node l x r) p
> goUp (Zipper r (WentRight l x p)) = Zipper (Node l x r) p

<p> And we might want to go all the way up: </p>

> allTheWayUp :: Zipper -> Tree
> allTheWayUp (Zipper t Top) = t
> allTheWayUp z              = allTheWayUp $ goUp z

<p> Now we'd like to check with QuickCheck that going down an arbitrary
path through a tree, then going all the way back up should bring
us back to the same tree. Make a data type that holds a tree and a
zipper: </p>

> data TreeAndZipper = TreeAndZipper Tree Zipper
>   deriving (Eq, Show)

<p> Now an instance of <code>Arbitrary</code> where the zipper is an arbitrary path through the tree: </p>

> instance Arbitrary TreeAndZipper where
>   arbitrary = do t <- arbitrary
>                  p <- arbPath $ create t
>                  return $ TreeAndZipper t p
>
>     where
>         arbPath z@(Zipper t p) = frequency [ (1, return z)    -- stop here
>                                            , (1, arbPath' z)  -- continue downwards
>                                            ]
>
>         arbPath' z@(Zipper Empty _) = return z
>         arbPath' z                  = frequency [ (1, arbPath $ goDownLeft  z)    -- go down left
>                                                 , (1, arbPath $ goDownRight z)    -- go down right
>                                                 , (1, return z)                   -- stop
>                                                 ]

<p> Now the property is that unrolling the zipper all the way gets us back to the original tree: </p>

> prop_down_up :: TreeAndZipper -> Bool
> prop_down_up (TreeAndZipper t z) = t == allTheWayUp z

<p> Check it: </p>

<pre>
*Zipper> quickCheck prop_down_up
+++ OK, passed 100 tests.
</pre>

<p> Using <code>verboseCheck</code> we can see some of the values: </p>

<pre>
(lots of output...)

TreeAndZipper (Node (Node (Node (Node (Node (Node (Node Empty (-7) (Node (Node (Node (Node Empty 88 (Node Empty (-79) Empty)) 82 (Node (Node Empty (-20) Empty) (-15) (Node Empty (-94) Empty))) (-60) Empty) 55 (Node Empty 0 Empty))) 6 (Node Empty (-7) Empty)) (-18) (Node Empty (-80) (Node Empty 60 Empty))) (-35) (Node Empty (-73) Empty)) (-32) (Node (Node (Node (Node (Node Empty (-71) Empty) 30 (Node (Node Empty 0 Empty) (-68) (Node Empty 91 Empty))) 1 (Node Empty (-46) (Node Empty (-41) (Node (Node Empty 93 Empty) 79 (Node (Node Empty 48 (Node (Node Empty 46 Empty) 76 (Node (Node Empty (-57) (Node Empty 90 Empty)) 34 (Node Empty (-11) (Node Empty (-10) Empty))))) 55 (Node Empty 65 (Node (Node (Node (Node Empty 2 (Node Empty 11 (Node Empty 34 Empty))) (-69) Empty) 68 Empty) 49 (Node Empty (-67) (Node (Node Empty 73 (Node Empty 59 (Node (Node Empty (-28) Empty) (-22) Empty))) (-15) Empty))))))))) 39 (Node Empty 40 (Node (Node (Node (Node Empty 88 Empty) 60 Empty) (-87) Empty) 53 Empty))) (-43) (Node Empty (-16) Empty))) 54 (Node Empty 73 Empty)) (-31) Empty) (Zipper (Node (Node (Node (Node (Node (Node Empty (-7) (Node (Node (Node (Node Empty 88 (Node Empty (-79) Empty)) 82 (Node (Node Empty (-20) Empty) (-15) (Node Empty (-94) Empty))) (-60) Empty) 55 (Node Empty 0 Empty))) 6 (Node Empty (-7) Empty)) (-18) (Node Empty (-80) (Node Empty 60 Empty))) (-35) (Node Empty (-73) Empty)) (-32) (Node (Node (Node (Node (Node Empty (-71) Empty) 30 (Node (Node Empty 0 Empty) (-68) (Node Empty 91 Empty))) 1 (Node Empty (-46) (Node Empty (-41) (Node (Node Empty 93 Empty) 79 (Node (Node Empty 48 (Node (Node Empty 46 Empty) 76 (Node (Node Empty (-57) (Node Empty 90 Empty)) 34 (Node Empty (-11) (Node Empty (-10) Empty))))) 55 (Node Empty 65 (Node (Node (Node (Node Empty 2 (Node Empty 11 (Node Empty 34 Empty))) (-69) Empty) 68 Empty) 49 (Node Empty (-67) (Node (Node Empty 73 (Node Empty 59 (Node (Node Empty (-28) Empty) (-22) Empty))) (-15) Empty))))))))) 39 (Node Empty 40 (Node (Node (Node (Node Empty 88 Empty) 60 Empty) (-87) Empty) 53 Empty))) (-43) (Node Empty (-16) Empty))) 54 (Node Empty 73 Empty)) (WentLeft Top (-31) Empty))
Passed:
TreeAndZipper (Node Empty (-33) Empty) (Zipper (Node Empty (-33) Empty) Top)
Passed:
TreeAndZipper Empty (Zipper Empty Top)
Passed:
TreeAndZipper (Node Empty (-95) Empty) (Zipper (Node Empty (-95) Empty) Top)
+++ OK, passed 100 tests.
</pre>

<p> Now a nifty thing about zippers is that we can use them to step through a traversal. If we are walking through a
tree, we might be finished, or we have produced a value (an <code>Int</code>) but need to keep going through the zipper: </p>

> data Step = Finished
>           | KeepGoing Int Zipper
>           deriving Show

> step :: Zipper -> Step

Empty tree, no context, so we're done.

> step (Zipper Empty Top) = Finished

We have gone down-left, so "emit" the x and return the rest of the zipper. (The runStep
function takes the x and continues the computation.)

> step (Zipper Empty (WentLeft  p x r)) = KeepGoing x (Zipper r p)

Some tree, some path, continue down-left.

> step (Zipper t p) = step $ goDownLeft (Zipper t p)

> inorder :: Tree -> [Int]
> inorder t = runStep' (step (Zipper t Top)) []
>   where
>     runStep' :: Step -> [Int] -> [Int]
>     runStep' Finished                    acc = acc
>     runStep' (KeepGoing x (Zipper t' p)) acc = runStep' (step (Zipper t' p)) (acc ++ [x])

> go :: [Int]
> go = inorder tree

> inorder' :: Tree -> [Int]
> inorder' Empty = []
> inorder' (Node l x r) = inorder' l ++ [x] ++ inorder' r

> prop_inorder :: Tree -> Bool
> prop_inorder t = inorder t == inorder' t











