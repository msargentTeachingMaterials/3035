import System.IO  
import Data.Char  
  
main = do  
    contents <- readFile "text.txt"     
    writeFile "testcaps.txt" (map toUpper contents) 
