import System.IO
import Control.Exception (bracket)

main = do
  handle <- openFile "test.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

main1 = do
  withFile "test.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr contents)

withFile1 :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile1 path mode f = bracket (openFile path mode) 
    (\handle -> hClose handle) 
    (\handle -> f handle)
