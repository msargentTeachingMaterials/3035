{-
*Solving problems with Haskell*

Reverse Polish Notation

We usually write math expressions like this:

10 - (4 + 3) * 2

with order of precedence decided by parentheses

But there is also Reverse Polish Notation (RPN)

10 4 3 + 2 * -

How is this evaluated? Go through the symbols left to right, placing numbers in a stack.
When a '+', '-', or '*' is reached, pop two from the stack, evaluate, then put the result
on the stack. The stack will eventually only have one number, the result. For the above the 
process is:

push 10, push 4, push 3; encounted '+', so pop 3 pop 4, add 4 3 = 7; push 7, push 2; 
encountered '*' so pop 2, pop 7, multiply 2 7 = 14; push 14; encountered '-' so pop 14, 
pop 10, subtract 10 14 = -4; push -4. 

At this point the list only had -4, so that's the answer.

Or, more succintly, go from left to right, when you find a symbol replace it with: the result
of applying it to those two numbers. Keep doing that until you have only one number

This yields:
10 4 3 + 2 * -
10 7 2 * -
10 14 -
-4

How to do this with a Haskell function? From the prompt or a file, we'd get a 
String. So suppose that all inputs would be Strings like this:

"10 4 3 + 2 * -"

Strategy: Since foldl moves from left to right and results in a single number that's the 
result of some cumulative process, it looks like the key. First we need a list, and need
to transform the above to:

["10","4","3","+","2","*","-"]

Then we foldl to reduce this to a single number that's the result of the calculation

Note, foldl applies a function to each element in a list along with an accumulator
that accumulates a running accumulation (sum, product, . . .). Here the accumulator 
will be a list that serves as a stack.

Sketching a function to do this:
  
import Data.List  
  
solveRPN :: (Num a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction stack item = ...  

words splits a string on " ", creating a list of words. 
[] is the starting accumulator (stack) value, and foldingFunction is the function
that does the work of updating the stack as we move through the list.

The work is now to define the function. See below for the answer


The solution:
-}

solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs  
{-

Note the use of patterns on the accumulator stack (x:y:ys) to get the top two 
Note that when a math operator is found, those top two are repaced by (x + y),
(x * y), etc.
Note that in the last pattern xs is the accumulator (we don't need to get into this one)
and the element is just a numberString: we just put the number onto the stack

Try:
ghci> solveRPN "10 4 3 + 2 * -"  
-4  
ghci> solveRPN "2 3 +"  
5  
ghci> solveRPN "90 34 12 33 55 66 + * - +"  
-3947  
ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
4037  
ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
4037  
ghci> solveRPN "90 3 -"  
87  

-}



