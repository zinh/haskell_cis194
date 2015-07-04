sumtorial :: Int -> Int
sumtorial n = sumtorial_tail n 0

sumtorial_tail :: Int -> Int -> Int
sumtorial_tail 0 s = s
sumtorial_tail n s = sumtorial_tail (n - 1) (s + n)

sumPair :: Int -> Int -> Int
sumPair a b = a + b

lsum :: [Int] -> Int
lsum [] = 0
lsum (h:tail) = h + lsum tail

llen :: [Int] -> Int
llen [] = 0
llen (_:tail) = 1 + llen tail

sumEveryTwo :: [Int] -> [Int]
sumEveryTwo [] = []
sumEveryTwo [a] = [a]
sumEveryTwo (a:b:tail) = (a + b):sumEveryTwo tail

hailstone :: Int -> Int
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

hailstoneLen :: Int -> Int
hailstoneLen 1 = 0
hailstoneLen n = 1 + hailstoneLen (hailstone n)

fizzbuzz :: Int -> [Char]
fizzbuzz n
  | n `mod` 15 == 0 = "fizz buzz"
  | n `mod` 3 == 0 = "fizz"
  | n `mod` 5 == 0 = "buzz"
  | otherwise = show n
