{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (intersperse)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P lst1) (P lst2) = lst1 == lst2
 
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P lst) 
      | poly == "" = "0"
      | otherwise = poly
      where poly = concat . intersperse "+" . reverse . filter (/="") . map setPower $ zip lst [0..]

setPower :: (Num a, Show a, Eq a) => (a, Integer) -> String
setPower (a, n)
  | a == 0 = ""
  | n == 0 = show a
  | n == 1 = (show a) ++ "x"
  | otherwise = (show a) ++ "x^" ++ (show n)

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined

