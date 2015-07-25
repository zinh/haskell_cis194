data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing = CNothing
  fmap f (CJust counter b) = CJust (counter + 1) (f b)

factorial :: Int -> Int
factorial 1 = 1
factorial a = a * factorial (a - 1)

factorial' :: Int -> Int -> Int
factorial' 1 total = total
factorial' a total = factorial' (a - 1) $ a*total

factorial1 :: Int -> Int
factorial1 n = foldl (*) 1 [1..n]

lastButOne :: [a] -> a
lastButOne [a, b] = a
lastButOne (x:xs) = lastButOne(xs)
