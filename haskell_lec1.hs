{- 
Haskell:
-A purely functional programming language
    * Variables are immutable, no program
    * state changes, no side effects
-Lazy, it evaluates a function only when it has to
-Programs are a series of transformations on data
-Reflects math more than the computer's architecture
-All you can do is calculate something and return the result
-Is compiled
-Is statically typed
-Has type inference
-Work on it began in 1987, the stable version of the language
-was defined in 2003 in the Haskell Report

-To get started, visit https://www.haskell.org/platform/ and
follow the instructions. It comes with a Haskell compiler and a
command line interactive mode (type ghci at your command line)

-To write a script make a file with a text editor and give it an
-.hs extension. 
-To load a script, navigate to the folder that has the file at your 
-prompt and type :l yourscriptnamewithoutdoths
-To reload a script, tpe :r

-When you get the interactive mode running you should see this:

-GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help  
-Loading package base ... linking ... done.  
-Prelude> 
-}

-- First, try out simple arithmetic, the functions min, max, comparisons
-- succ, boolean algebra, backtics

--Try some functions: define them here, then load this file

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                        then x
                        else x*2 -- else is manditory, as it is an
--expression and always returns something
doubleSmallNumberConcise x = (if x > 100 then x else x*2)

area x = 3.14 * x * x

hypo a b = a*a + b*b
{-
Lists:

- Are homogeneous in Haskell
- You can set a variable to a list with the let keyword 
(let is not necessary in a script)
- for example (try it at the prompt):
let lostNumbers = [4,8,15,16,23,42]

- to see the value, just type 
lostNumbers  

- You can use list literals, and add them with ++ (try these at the prompt)
[1,2,3,4] ++ [9,10,11,12]
"hello" ++ " " ++ "world"  

- When you add two lists, Haskell has to traverse the list starting from 
left to right to put the second list at the end, so watch adding
long strings

- Lists are implemented with singly-linked lists by Haskell

- You can compare lists (try all of these)

[3,2,1] > [2,1,0] 
[3,2,1] > [2,10,100]
[3,4,2] > [3,4] 
[3,4,2] == [3,4,2]  

- The head of a list can be found with the head function. The head is the first
element

head [5,4,3,2,1]  

- The tail of a list, all the elements after the first one, can be gotten
with the tail

tail [5,4,3,2,1]  

- last gives you the last element, init gives you everything but the last element

last [5,4,3,2,1] 
init [5,4,3,2,1]  

- btw head of empty lists causes an exception

- More built-in list functions: 
length gives you the length 
null checks if it's null
reverse reverses it
take n will take the first n elements from the list
minimum gets the minimum element
maximum gets the maximum element
sum sums all the elements together
product multiplies all the elements together
elem checks if the first element is an element of the second (a list)

Btw, for 2 place functions, you can use back-ticks ` to make them inline
for example:
4 `elem` [3,4,5,6]

- Ranges
[n..m] gives you all the values starting with n and ending with m
try [3..40]
['a'..'z']

- You can get more complicated patterns by giving Haskell the first two elements, 
and the ending element, and it figures it out

[2,4..100]

You can also do this to go backward
[20,19..1]

And you can do it with floats
[0.1, 0.2 .. 2]

- A couple more functions
cycle [1,2,3]
take 10 (cycle [1,2,3])
repeat 5
take 4 (repeat 5)

-}
