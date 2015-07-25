import Data.Char
import Data.List

main = do line <- getLine
          let line' = reverse line
          putStrLn $ "You said " ++ line' ++ " backwards!"
          putStrLn $ "Yes, you said " ++ line' ++ " backwards"

main2 = do line <- fmap reverse getLine
           putStrLn $ "You said " ++ line ++ " backwards!"
           putStrLn $ "Yes, you said " ++ line ++ " backwards"

main3 = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
           putStrLn line
