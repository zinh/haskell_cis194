data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

intInts :: Monoid m => (Integer -> m) -> m
intInts mk_m = go [1..100]
  where go [] = mempty
        go (n:ns)
          | let div_by_5 = n `mod` 5 == 0
                div_by_7 = n `mod` 7 == 0
            in
              (div_by_5 || div_by_7) && (not (div_by_5 && div_by_7)) = mappend (mk_m n) (go ns)
          | otherwise = go ns
