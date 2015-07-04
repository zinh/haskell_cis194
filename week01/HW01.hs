{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n 
  | n > 0 = n `mod` 10 : toRevDigits (n `div` 10)
  | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = doubleEveryOtherPos lst 1

doubleEveryOtherPos :: [Integer] -> Integer -> [Integer]
doubleEveryOtherPos [] _ = []
doubleEveryOtherPos (x:xs) index
  | index `mod` 2 == 0 = x*2 : doubleEveryOtherPos xs (index + 1)
  | otherwise = x : doubleEveryOtherPos xs (index + 1)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits lst = sumDigits' lst 0

sumDigits' :: [Integer] -> Integer -> Integer
sumDigits' [] total = total
sumDigits' (h:t) total = sumDigits' t (total + sum (toRevDigits h))


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n =
  let s = sumDigits (doubleEveryOther (toRevDigits n))
  in
  case s `mod` 10 of
    0 -> True
    _ -> False

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
