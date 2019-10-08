 {-
*Command line arguments*

With command line arguments we can pass information into a program 
before it runs, and not have to interact with it after it starts running.
	- good for batch scripts

To use command line arguments, we use the module:
System.Environment

It has two IO actions we can use: getArgs and getProgName

A short demo of these functions:

-}
{-
import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName 

-}
{-
Bind args to getArgs
bind progName to getProgName
Then print to screen


Another app using these functions: todo
It views, adds, and deletes tasks

to use, call the program with:
function filename stringtoadd

-}

import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
-- dispatch is an association list, a lot like a Python dictionary
   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  

-- lookup gets a value for a key in an association list
  
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
-- appendFile is an IO action that appends a string to a file
  
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  

-- lines: breaks up strings into a list of strings at the new line char
-- unlines: reverse of lines
  
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    contents <- readFile fileName   
    (tempName, tempHandle) <- openTempFile "." "temp"  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems   
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  

-- openTempFile creates a temporary file, which must be removed manually
-- removeFile is a function from System.Directory that removes a file
-- we are using a temp file to store intermediate results, since the 
-- text doesn't mention a way to directly remove contents from a file in Haskell 
-- ReadMode opens the file in a read only fashion: ok since we aren't modifying it
