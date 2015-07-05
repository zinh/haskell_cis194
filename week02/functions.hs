lzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
lzipWith _ [] _ = []
lzipWith _ _ [] = []
lzipWith f (x:xs) (y:ys) = f x y : lzipWith f xs ys

lmap :: (a -> b) -> [a] -> [b]
lmap _ [] = []
lmap f lst = lmap' f lst []

-- tail recursion version
lmap' :: (a -> b) -> [a] -> [b] -> [b]
lmap' _ [] mapped = reverse mapped
lmap' f (x:xs) mapped = lmap' f xs (f x : mapped)

lfilter :: (a -> Bool) -> [a] -> [a]
lfilter _ [] = []
lfilter f (x:xs)
  | f x = x : lfilter f xs
  | otherwise = lfilter f xs

collatz :: Int -> Int
collatz 1 = 0
collatz n
  | even n = 1 + collatz (n `div` 2)
  | otherwise = 1 + collatz (n*3 + 1)
