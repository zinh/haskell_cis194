import qualified Data.Map as Map
findKey :: (Eq k) => [(k,v)] -> k -> Maybe v
findKey [] _ = Nothing
findKey ((k,v):xs) key
  | k == key = Just v
  | otherwise = findKey xs key
