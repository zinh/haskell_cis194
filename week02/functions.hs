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

lfoldl :: (b -> a -> b) -> b -> [a] -> b
lfoldl _ acc [] = acc
lfoldl f acc (x:xs) = lfoldl f (f acc x) xs

sum' :: (Num a) => [a] -> a
sum' = foldl (\acc x -> acc + x) 0

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50

-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
