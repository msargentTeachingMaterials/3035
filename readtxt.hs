import System.IO  
  
main = do  
    contents <- readFile "text.txt"  
    putStr contents  