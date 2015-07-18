solveRPN :: String -> Double
solveRPN expr = head . rpn $ words expr

rpn :: [String] -> [Double]
rpn = foldl calc []

calc :: [Double] -> String -> [Double]
calc acc x
  | x `elem` ["+", "-", "*", "/"] = doCalc x acc
  | otherwise = (read x :: Double):acc

doCalc :: String -> [Double] -> [Double]
doCalc op (a:b:xs) = 
  let val = case op of
              "+" -> a + b
              "-" -> a - b
              "*" -> a * b
              "/" -> a / b
              _ -> 0
  in
    val:xs
