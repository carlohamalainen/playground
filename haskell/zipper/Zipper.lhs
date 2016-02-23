<p> This note about zippers follows <a href="https://www.lri.fr/~filliatr/publis/enum2.pdf">Backtracking Iterators (Jean-Christophe Filliâtre)</a>. The paper
has examples in OCaml but they translate to Haskell fairly directly.
Literate Haskell source for this post is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/zipper">https://github.com/carlohamalainen/playground/tree/master/haskell/zipper</a>.
To run this file, first install QuickCheck: </p>

<pre>
cabal update
cabal install QuickCheck
</pre>

> module Zipper where
>
> import Debug.Trace
> import Test.QuickCheck
> import Test.QuickCheck.Gen

<h3>A tree datatype</h3>

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
- we are at the top of the tree.  Otherwise we have gone down
the left subtree or the right subtree. </p>

<p> If we went down the left branch at a node,
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

<p> Encode this information in a type: </p>

> data Path = Top                      -- ^ No path.
>           | WentLeft  Path Int Tree  -- ^ Followed the left subtree
>           | WentRight Tree Int Path  -- ^ Followed the right subtree
>           deriving (Eq, Show)

<p> A zipper is a tree with a path. </p>

> data Zipper = Zipper Tree Path
>             deriving (Eq, Show)

<h3> Working with zippers </h3>

<p> The initial zipper is just the tree with no path. </p>

> createZipper :: Tree -> Zipper
> createZipper t = Zipper t Top

<p> Conversely, if we have a zipper and we are at the top,
we can get the tree out of it. </p>

> unZipper :: Zipper -> Tree
> unZipper (Zipper t Top) = t
> unZipper (Zipper t p)   = error $ "Can't unZipper here, path is " ++ show p ++ " with tree " ++ show t

<p> Intuitively, we would expect that <code>unZipper . createZipper = id</code>, and we can check this
using <a href="https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck.html">QuickCheck</a>. First,
provide an <code>instance</code> of <code>Arbitrary</code> for our binary trees: </p>

> instance Arbitrary Tree where
>   arbitrary = frequency [ (1, return Empty) -- Empty
>                         , (1, arbNode)      -- Node <left> <n> <right>
>                         ]
>       where arbNode = do l <- arbitrary   -- <left>
>                          n <- arbitrary   -- <n>
>                          r <- arbitrary   -- <right>
>                          return $ Node l n r

<p> Now the property <code>unZipper . createZipper = id</code> can be written as: </p>

> prop_finish_createZipper t = unZipper (createZipper t) == t

<p> Check it: </p>

<pre>
*Zipper> quickCheck prop_finish_create
+++ OK, passed 100 tests.
</pre>

<p> Looks good. Use <code>verboseCheck prop_finish_create</code> to see the values being generated. </p>

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

<p> Going up is the inverse of <code>goDownLeft</code> and <code>goDownRight</code>.</p>

> goUp :: Zipper -> Zipper
> goUp (Zipper Empty Top)           = Zipper Empty Top
> goUp (Zipper l (WentLeft  p x r)) = Zipper (Node l x r) p
> goUp (Zipper r (WentRight l x p)) = Zipper (Node l x r) p

<p> And we might want to go all the way up: </p>

> unzipZipper :: Zipper -> Tree
> unzipZipper (Zipper t Top) = t
> unzipZipper z              = unzipZipper $ goUp z

<p> Now we'd like to check with QuickCheck that going down an arbitrary
path through a tree, then going all the way back up should bring
us back to the same tree. So we will have to create random trees, paired
with random paths through those trees. A tuple of type <code>(Tree, Zipper)</code>
could work, but runs into dramas with overlapping instances since QuickCheck provides an instance
for types, namely <code>Arbitrary (a, b)</code>. </p>

<p> As a work-around, make a data type that holds a tree and a zipper: </p>

> data TreeAndZipper = TreeAndZipper Tree Zipper
>   deriving (Eq, Show)

<p> Here is the instance of <code>Arbitrary</code>: </p>

> instance Arbitrary TreeAndZipper where
>   arbitrary = do t <- arbitrary                 -- an arbitrary tree t
>                  p <- arbPath $ createZipper t  -- an arbitrary path in t
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

<p> Now with this instance we can encode the test that going down in a tree and then back up brings us back to the same tree. </p>

> prop_zip_unzip :: TreeAndZipper -> Bool
> prop_zip_unzip (TreeAndZipper t z) = t == unzipZipper z

<p> Check it: </p>

<pre>
*Zipper> quickCheck prop_zip_unzip
+++ OK, passed 100 tests.
</pre>

<p> Using <code>verboseCheck</code> we can see some of the values. Here is a sample: </p>

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

<h3> Traversals with a zipper </h3>

<p> A nifty thing about zippers is that we can use them to step
through a traversal, controlling the process programatically. If we
are walking through a tree, we might be finished, or we have produced
a value (an <code>Int</code>) but need to keep going through the
zipper: </p>

> data Step = Finished
>           | KeepGoing Int Zipper
>           deriving Show

<p> The <code>step</code> function converts a zipper into this state (step) type: </p>

> step :: Zipper -> Step

<p> If we have an empty tree and no path, we are done. </p>

> step (Zipper Empty Top) = Finished

<p> If we have gone down-left, make note of the node's value <code>x</code> and the rest of the zipper: </p>

> step (Zipper Empty (WentLeft  p x r)) = KeepGoing x (Zipper r p)

<p> Otherwise, we have a tree and a path, so try to continue by going down-left: </p>

> step (Zipper t p) = step $ goDownLeft (Zipper t p)

<p> In summary: </p>

< step :: Zipper -> Step
< step (Zipper Empty Top)               = Finished
< step (Zipper Empty (WentLeft  p x r)) = KeepGoing x (Zipper r p)
< step (Zipper t p)                     = step $ goDownLeft (Zipper t p)

<p> By repeatedly applying <code>step</code> we get an inorder traversal of the tree: </p>

> inorder :: Tree -> [Int]
> inorder t = runStep (step (Zipper t Top)) []
>   where
>     runStep :: Step -> [Int] -> [Int]
>     runStep Finished                    acc = acc
>     runStep (KeepGoing x (Zipper t' p)) acc = runStep (step (Zipper t' p)) (acc ++ [x])

<p> (As an aside, <code>runStep</code> is tail recursive.) </p>

<p> Using <code>inorder</code> on our example tree: </p>

<pre>
*Zipper> inorder tree
[1,2,3,4]
</pre>

<p> Here is a plain recursive definition of an inorder traversal: </p>

> inorder' :: Tree -> [Int]
> inorder' Empty = []
> inorder' (Node l x r) = inorder' l ++ [x] ++ inorder' r

<p> We can use this to verify that our fancy zipper inorder traversal is correct: </p>

> prop_inorder :: Tree -> Bool
> prop_inorder t = inorder t == inorder' t

<p> Testing it: </p>

<pre>
*Zipper> quickCheck prop_inorder
+++ OK, passed 100 tests.
</pre>

<p> If we want to do something different in the traversal,
for example running a monadic action, we can use the same <code>Step</code> datatype
and change the definition of <code>runStep</code>: </p>

> inorderM :: Monad m => (Int -> m a) -> Tree -> m ()
> inorderM a t = runStepM a $ step (Zipper t Top)
>   where
>     runStepM :: Monad m => (Int -> m a) -> Step -> m ()
>     runStepM _ Finished                    = return ()
>     runStepM a (KeepGoing x (Zipper t' p)) = (a x) >> (runStepM a $ step (Zipper t' p))

<p> Example usage: </p>

<pre>
*Zipper> inorderM (\x -> putStrLn $ "Node value: " ++ show x) tree
Node value: 1
Node value: 2
Node value: 3
Node value: 4
</pre>

<h3> Mapping over a tree </h3>

<p> If we want to apply a function to each
value in a tree, a recursive definition might be: </p>

> mapTree :: (Int -> Int) -> Tree -> Tree
> mapTree _ Empty = Empty
> mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

<pre>
*Zipper> tree
Node (Node Empty 1 Empty) 2 (Node Empty 3 (Node Empty 4 Empty))

*Zipper> mapTree (+1) tree
Node (Node Empty 2 Empty) 3 (Node Empty 4 (Node Empty 5 Empty))
</pre>

<p> We can check that <code>mapTree id == mapTree</code>: </p>

> prop_maptree :: Tree -> Bool
> prop_maptree t = t == (mapTree id t)

<pre>
*Zipper> quickCheck prop_maptree
+++ OK, passed 100 tests.
</pre>

<p> We can also use a zipper to map over the tree by using a different
data type to represent the stepping: </p>

> data MapStep = MapFinished
>              | MoreL Int Zipper
>              | More2 Zipper Int Zipper
>              deriving Show

> stepMap :: (Int -> Int) -> Zipper -> MapStep
> stepMap _ (Zipper Empty Top              ) = MapFinished
> stepMap f (Zipper Empty (WentLeft  p x r)) = MoreL (f x) (Zipper r p)
> stepMap f (Zipper (Node l x r) p)          = More2 (Zipper l p) (f x) (Zipper r p)

> mapTree' :: (Int -> Int) -> Tree -> Tree
> mapTree' f t = runStep (stepMap f $ Zipper t Top)
>   where
>     runStep :: MapStep -> Tree
>     runStep MapFinished     = Empty
>     runStep (MoreL x z)     = Node Empty x (runStep $ stepMap f z)
>     runStep (More2 zl x zr) = Node (runStep $ stepMap f zl) x (runStep $ stepMap f zr)

<p> Testing it: </p>

<pre>
*Zipper> tree
Node (Node Empty 1 Empty) 2 (Node Empty 3 (Node Empty 4 Empty))

*Zipper> mapTree' (+1) tree
Node (Node Empty 2 Empty) 3 (Node Empty 4 (Node Empty 5 Empty))
</pre>

<p> And testing it using QuickCheck: </p>

> prop_maptree' :: Tree -> Bool
> prop_maptree' t = (mapTree (+1) t) == (mapTree' (+1) t)

<pre>
*Zipper> quickCheck prop_maptree'
+++ OK, passed 100 tests.
</pre>

<p> The <code>Main.hs</code> file runs all the tests: </p>

<pre>
$ ghc --make Main.hs
[1 of 2] Compiling Zipper           ( Zipper.lhs, Zipper.o )
[2 of 2] Compiling Main             ( Main.hs, Main.o )
Linking Main ...

$ ./Main
prop_finish_createZipper
+++ OK, passed 100 tests.
prop_inorder
+++ OK, passed 100 tests.
prop_maptree
+++ OK, passed 100 tests.
prop_maptree'
+++ OK, passed 100 tests.
prop_zip_unzip
+++ OK, passed 100 tests.
</pre>
