
> module Zipper where

Following: https://www.lri.fr/~filliatr/publis/enum2.pdf

Balanced binary tree, storing integers:

> data Tree = Empty | Node Tree Int Tree
>           deriving Show

> data Path = Top                      -- ^ No context.
>           | WentLeft  Path Int Tree  -- ^ Followed the left subtree; tree is the right subtree.
>           | WentRight Tree Int Path  -- ^ Followed the right subtree; tree is the left subtree.
>           deriving Show

Initial zipper is just the tree with no context.

> create :: Tree -> (Tree, Path)
> create t = (t, Top)

> finish :: (Tree, Path) -> Tree
> finish (t, Top) = t
> finish (t, p)   = error $ "Can't finish here, context is " ++ show p ++ " with tree " ++ show t

Quickcheck: finish . create = id

> goDownLeft :: (Tree, Path) -> (Tree, Path)
> goDownLeft (Empty, _)      = error "Can't go down-left on an empty tree."
> goDownLeft (Node l x r, p) = (l, WentLeft p x r)

> goDownRight :: (Tree, Path) -> (Tree, Path)
> goDownRight (Empty, _)      = error "Can't go down-right on an empty tree."
> goDownRight (Node l x r, p) = (r, WentRight l x p)


> setValue :: (Tree, Path) -> (Int -> Int) -> (Tree, Path)
> setValue (_, Top) _ = error "Can't apply function with an empty context."
> setValue (t, WentLeft  p x r) f = (t, WentLeft  p (f x) r)
> setValue (t, WentRight l x p) f = (t, WentRight l (f x) p)

> step :: (Tree, Path) -> Either () (Int, (Tree, Path))

Empty tree, no context, so we're done.

> step (Empty, Top)             = Left ()

We have gone down-left, so "emit" the x and return the rest of the zipper. (The runStep
function takes the x and continues the computation.)

> step (Empty, WentLeft  p x r) = Right (x, (r, p))

Some tree, some path, continue down-left.

> step (t, p)                   = step $ goDownLeft (t, p)

> inorder :: Tree -> [Int]
> inorder t = runStep' (step (t, Top)) []
>   where
>     runStep' :: Either () (Int, (Tree, Path)) -> [Int] -> [Int]
>     runStep' (Left  ())          acc = acc
>     runStep' (Right (x, (t, p))) acc = runStep' (step (t, p)) (acc ++ [x])

> tree01 :: Tree
> tree01 = Node
>           (Node Empty 1 Empty)
>           2
>           (Node Empty 3 (Node Empty 4 Empty))


> go = inorder tree01

