lmap :: (a -> b) -> [a] -> [b]
lmap _ [] = []
lmap f (x:xs) = f x : lmap f xs

head' :: [a] -> a
head' [] = error "Empty list!"
head' (x:_) = x

maximum' :: (Ord a) => [a] -> a
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

take' :: Int -> [a] -> [a]
take' n (x:xs) = x : take' (n - 1) xs

revert :: [a] -> [a]
revert [] = []
revert (x:xs) = revert xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a

lzip :: [a] -> [b] -> [(a, b)]
lzip [] _ = []
lzip _ [] = []
lzip (x:xs) (y:ys) = (x,y) : lzip xs ys

lelem :: (Eq a) => a -> [a] -> Bool
lelem _ [] = False
lelem a (x:xs)
  | a == x = True
  | otherwise = lelem a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let (left, right) = pivot x xs [] []
  in quicksort left ++ [x] ++ quicksort right

pivot :: (Ord a) => a -> [a] -> [a] -> [a] -> ([a], [a])
pivot _ [] left right = (left, right)
pivot p (x:xs) left right
  | p < x = pivot p xs left (x:right)
  | otherwise = pivot p xs (x:left) right

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
  let left = [m | m <- xs, m <= x]
      right = [m | m <- xs, m > x]
  in quicksort' left ++ [x] ++ quicksort' right
