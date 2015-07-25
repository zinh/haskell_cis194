data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = (Node a) (treeInsert x left) right
  | x > a = (Node a) left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x > a = treeElem x right
  | x < a = treeElem x left

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

data TrafficLight = Red | Green | Yellow
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Green = "Green light"
  show Yellow = "Blue light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True
