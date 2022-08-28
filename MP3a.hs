module MP3a where

import Data.List
import Data.List.Split


{-
  Binary tree related type definitions.
-}
data BinTree a = Node a (BinTree a) (BinTree a) deriving Show
data Direction = L | R deriving (Show, Eq)


instance Eq a => Eq (BinTree a) where
  (==) = eqApprox 10
    where
      -- Can't compare infinite structures, so test only first 10 levels
      eqApprox :: (Eq a) => Int -> BinTree a -> BinTree a -> Bool
      eqApprox 0 _ _  = True
      eqApprox n (Node x l0 r0) (Node y l1 r1) = x == y && eqApprox (n-1) l0 l1 && eqApprox (n-1) r0 r1



{-
  Creates a `BinTree` where all nodes contain the specified value.
-}
treeRepeat :: a -> BinTree a
treeRepeat num = binaryTree
      where binaryTree = Node num (binaryTree) (binaryTree)


{-
  Creates a `BinTree` where the nodes are populated with the natural numbers,
  starting at the "root" of the tree and then downwards and from left to right
  across each level of the tree.
-}
treeNats :: BinTree Integer
treeNats = treeNats' 0
  where
    treeNats' n = Node (n + 1) (treeNats' $ n * 2 + 1) (treeNats' $ n * 2 + 2)

{-
  Takes a list of `Direction` values (`L`eft or `R`ight) and traverses the tree
  to return the value in the target node.

  Examples:

  treeVal [L,R] treeNats => 5

  treeVal [] treeNats => 1
-}
treeVal :: [Direction] -> BinTree a -> a
treeVal [] (Node v _ _) = v
treeVal (d:ds) (Node _ l r) = if d == L then (treeVal ds l) else (treeVal ds r)


{-
  Converts a tree to a list; the root of the tree is the first list value, and
  the values in the tree are taken downwards and across each level.

  Examples:

  take 10 $ treeToList treeNats
  => [1,2,3,4,5,6,7,8,9,10]
-}
treeToList :: BinTree a -> [a]
treeToList t = [treeVal l t | l <- (concat $ [sequence (replicate i [L,R]) | i <- [0..]])]


{-
  "Flips" the `BinTree` so that we obtain the mirror image of the original tree.

  For instance, flipping the tree on the left gives us the one on the right:

             1                     1
           /   \                 /   \
          2     3      =>       3     2
         / \   / \             / \   / \
        4   5 6   7           7   6 5   4
-}
treeFlip :: BinTree a -> BinTree a
treeFlip (Node x l r) = Node x (treeFlip r) (treeFlip l)


{-
  Returns a `BinTree` based on an infinite list where the first item of the list
  is the root, and subsequent items from the list are assigned to nodes
  downwards and across the levels of the tree.

  Examples:

  take 10 $ treeToList $ treeFromList [1..]
  => [1,2,3,4,5,6,7,8,9,10]

  Hint: check out your `treeNats` for inspiration!
-}
treeFromList :: [a] -> BinTree a
treeFromList l = (l!!) . pred . fromIntegral <$> treeNats


{-
  Takes a function and an initial value, and returns a `BinTree` where the root
  value is the initial value, and values in subsequent nodes are based on
  repeated applications of the given function to the value.

  Examples:

  treeVal [R,R,R] $ treeIterate (2*) 1
  => 16384

  take 15 $ treeToList $ treeFlip $ treeIterate (2*) 1
  => [1,4,2,64,32,16,8,16384,8192,4096,2048,1024,512,256,128]

  Hint: checkout `iterate`.
-}
treeIterate :: (a -> a) -> a -> BinTree a
treeIterate f g = treeFromList $ iterate f g


{-
  BinTree instance of the Functor class.
-}

instance Functor BinTree where
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

{-
  BinTree instance of the Applicative class.
-}
instance Applicative BinTree where
  pure x = Node x (pure x) (pure x)
  (Node f lf rf) <*> (Node x l r) = Node (f x) (lf <*> l) (rf <*> r)
