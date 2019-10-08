import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "text.txt"     
    writeFile "textcaps.txt" (map toUpper contents) 