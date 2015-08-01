{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c1 c2 = sum $ zipWith (\x y -> if x == y then 1 else 0) c1 c2

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\color -> foldl (\acc x -> if x == color then acc + 1 else acc) 0 code ) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches code guest = sum $ zipWith min (countColors code) (countColors guest)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guest = 
  let exactMatch = exactMatches secret guest
      match = matches secret guest
  in Move guest exactMatch $ match - exactMatch

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code exactMatch nonexactMatch) guest = 
  let Move _ exact nonexact = getMove code guest
  in (exact == exactMatch) && (nonexact == nonexactMatch)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Create code with n length from initial codes of n - 1 length
createMoves :: [Code] -> [Code]
createMoves undefined

singleMove :: Code -> [Code]
singleMove code = concatMap (\color -> ) colors

insertColor :: Code -> Peg -> [Code]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
