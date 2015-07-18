### Type system

To define a new data type

```haskell
data Bool = True | False

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

`Shape`: type
`Circle`, `Rectangle`: value contructor

```
> :t Circle
Circle :: Float -> Float -> Float -> Shape

> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

Let write a function using Shape

```haskell
area :: Shape -> Float
area (Circle _ _ radius) = pi * radius
area (Rectangle a1 a2 b1 b2) = (abs $ a2 - a1) * (abs $ b2 - b1)
```

Deriving a type class

```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
```

To export a type

```haskell
module Shapes
(Point(..)
, Shape(..)
) where
```

Record syntax

```haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
```

Type parameter

```haskell
data Maybe a = Just a | Nothing
```

we call `Maybe` *type constructor*

Type synonyms

```haskell
type String = [Char]
```

Parameterizing Type Synonyms

```haskell
type IntMap v = Map Int v

-- or we can write
type IntMap = Map Int
```

Recursive Data Structures

```haskell
-- A binary tree type
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

-- Helper method
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Insert a value to a tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)
```

Usage:

```
> let numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]
```

Define a type class

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

To derive `Eq` type class

```haskell
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
```

`Maybe` derive from `Eq`, note the type constraint

```haskell
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```

### IO

Output string

```haskell
main = putStrLn "Hello world"
```

```
> :t putStrLn
putStrLn :: String -> IO ()
```

Input a string

```haskell
main = do
  name <- getLine
  putStrLn $ "Hello" ++ name
```

```
> :t getLine
getLine :: IO String
```

Use let inside an IO action

```haskell
import Data.Char
main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " "
                    ++ bigLastName
                    ++ ", how are you?"
```

Another example

```haskell
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
```

Note that return keyword will not terminate the execution


