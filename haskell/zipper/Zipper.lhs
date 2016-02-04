
> module Zipper where

Following: https://www.lri.fr/~filliatr/publis/enum2.pdf

<p> Balanced binary tree, storing integers: </p>

> data Tree = Empty | Node Tree Int Tree
>           deriving Show

<p> Example tree: </p>

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
>           deriving Show

<p> A zipper is a tree and the path. </p>

> data Zipper = Zipper Tree Path
>             deriving Show

<p> The initial zipper is just the tree with no path. </p>

> create :: Tree -> Zipper
> create t = Zipper t Top

<p> Conversely, if we have a zipper and we are at the top,
we can get the tree out of it. </p>

> finish :: Zipper -> Tree
> finish (Zipper t Top) = t
> finish (Zipper t p)   = error $ "Can't finish here, path is " ++ show p ++ " with tree " ++ show t

Quickcheck: finish . create = id

<p> Walking into the left subtree, as in the example above, involves
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

