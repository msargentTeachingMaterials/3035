{-
**IO and Compiling a Haskell Program**
Note: this code won't compile, it's just given a hs extension for syntax 
coloring


Setup:

If you are using windows 10, follow the instructions
at this link: 
https://downloads.haskell.org/~ghc/5.04/docs/html/users_guide/sec-install-windows.html
If you are using Mac or Linux, do nothing.

Try putting this code into a separate haskell file, hello_world.hs
-}
main = putStrLn "hello, world" 

{- 
Then open your linux or mac terminal, navigate to the folder
that has your haskell file, and type this (except the $, that's 
just the prompt, don't ever copy that for terminal commands):

$ ghc --make hello_world 



Then try:
$ ./helloworld 
You should see "hello, world" displayed at the prompt

Consider this code:

ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO ()

IO action is a type with a result type that comes after the IO
() is the empty tuple, a dummy value

Now try this in a new haskell file and compile like we did before
It asks you for input, put it in and see the result
-}
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!") 

{-
main here is like the main method in Java

We are adding some impure parts to Haskell in this module:
code that has side effects, code that looks more like
imperative code. But we can keep the impure parts separated
from the pure parts. Anything with IO in front of it is an IO
action and is part of the impure code in a Haskell program.

Now try doing the same as above with this code:
-}
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words

{-
Here we separate the pure from the impure: reverseWords is pure
functional code, the rest is not

main's type signature
main :: IO <some type>

name <- getLine 
is a way to get input from getLine:

Also, see:
ghci> :t getLine  
getLine :: IO String  

It doesn't return a string, but an IO action with a string type. 
To get something out of an IO action use the <- construct

The computation inside the IO object is tainted, but it is untainted 
when it is bound to a name with the <- construct, and is then immutable.

Another program to try:
-}
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name  

{-
The tellFortune need not know where the name came from, once name is bound, 
it's part of the pure functional code

Question: Will this work:
-}

nameTag = "Hello, my name is " ++ getLine  

{-
The answer is no, for getLine is impure, and you can't mix it with pure code
like nameTag = "Hello .. " ++

Get the stuff out of getLine first using <-, bind it, and then you can 
use the value

All of this is to limit side effects to only certain parts of the code

BTW, this won't do what you want either:

name = getLine

for the same reasons. It just creates an alias for getLine.

Another note: In a script, IO actions can only be performed when 
given the name of main or when they are inside a bigger IO option 
composed with a do block. They always eventually fall under main.

Let expressions can be used without in when inside do blocks:
-}
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 

{-
Note that the do lines are indented and lined up, and the let lines are also 
indented and lined up

Here's another example:
-}

main = do   
    line <- getLine  
    if null line  
        then return () 
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words  

{-
return here doesn't do what it does in imperative languages, it just takes
a value and puts into an IO action. In our code, this action is carried out 
(nothing is printed). Since there is nothing else to do in this branch, the 
program stops.

Note a recursive call to main when something is entered: this keeps the loop
going for further entries

Check out, from the textbook, putStr, putChar, getChar,  print: these are helpful 
functions.

*File IO*

getContents reads everything from a standard input until it reaches an
end-of-file character.

It works lazily, only gets what you ask for from the input

Retrieve values from getContents with <-

Useful with piping (a UNIX thing):

To see what that is, make a text file with some text (use this if you want)

 So that it must, 
 in fine, be maintained, 
 all things being maturely 
 and carefully considered, 
 that this proposition, 
 I am, I exist, 
 is necessarily true each 
 time it is expressed by me, 
 or conceived in my mind.

 Then make a Haskell file named capslocker.hs and put this in it:
-}
import Control.Monad  
import Data.Char  
  
main = forever $ do  
    l <- getLine  
    putStrLn $ map toUpper l  

{-
Then try this at the command line:

$ ghc --make capslocker   
$ cat text.txt    
$ cat text.txt | ./capslocker  

Here we are piping the result printing of capslocker into

Here's away to read a file directly, without using cat:  
-}

import System.IO  
  
main = do  
    contents <- readFile "test.txt"  
    putStr contents  

{- an easy way to write to a file
-}

import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "text.txt"     
    writeFile "textcaps.txt" (map toUpper contents) 

{-
These are the simplest ways to do file IO. Learn You a Haskell has other,
more complicated and flexible ways as well, it might be worth taking a look
when you have time. 

-} 




