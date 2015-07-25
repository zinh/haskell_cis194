data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton value = Node value Empty Empty

buildTree :: (Eq a, Ord a) => a -> Tree a -> Tree a
buildTree value Empty = singleton value
buildTree value t@(Node root left right)
  | value == root = t
  | value > root = Node root left $ buildTree value right
  | value < root = Node root (buildTree value left) right

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
