{-
**Recursion**

Recursion: functions that use themeselves in their definitions

Recursive functions have to end at some point, to do this, there 
must be values that get passed into the function that are 
not defined recursively: the value is determined directly. 
These are base cases.

Strategy: 
Define a base case. 
Then, for non-base cases, define a way to get the total 
solution from a smaller version of the problem (the recursive case). 
This last part is called divide-and-conquer

A feature of Haskell (functional programming): "Recursion is 
important to Haskell because unlike imperative languages, you 
do computations in Haskell by declaring what something is instead 
of declaring how you get it. "

There are no loops in Haskell!

Some examples of recursion:

Let's define our own maximum in Haskell, recursively

Base case (list with one element): maximum of [a] is a
Recursive case: maximum x:the_rest will be:
x if it is greater than the max of the rest of the list
Otherwise, it will be the max of the rest of the list

How do we find the max of the rest of the list? Call maximum on it!
It will break the problem down another level, until it gets to base
cases

Walking through it:

maximum of [4, 2, 5, 8] is:
max of 4, maximum [2, 5, 8], which is:
		  max of 2, maximum [5, 8], which is
		            max 5, maximum [8], which is
		            	max 5, 8, which is 8. Then
		  max of 2, 8 is 8, Then
max of 4, 8 is 8
Then we return 8

Here's the definition in Haskell
-}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "list empty"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- Or even clearer, using the included max fcn
maximum2 :: (Ord a) => [a] -> a
maximum2 [] = error "list empty"
maximum2 [x] = x
maximum2 (x:xs) = max x (maximum2 xs)

{-
Another example: replicate. It takes an Int n and some
element e (we'll make it an Ord) and returns a list containing 
e repeated n times
-}
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0  = []
    | otherwise = x:replicate' (n-1) x


-- Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- repeat a character
repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip two lists
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

-- our own elem
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs


{-
*Quicksort*

Review your algorithms class material for how the sorting 
algorithm works

An invariant: each sorted list is such that for any 
sublist in the list, all the elements to the left of the 
middle element are less than or equal to the middle element, and
all of the elements to the right of it are greater than or 
equal to it. Specify these relationships recursively in our function

That is, a sorted lists is the concatentation of:
sorted list of all elements <= e ++ e ++ sorted list of elements > e


Here's the algorithm in Haskell
-}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x]
        

-- remember, we are not telling Haskell how to calculate this,
-- we are just informing it of all the relevant relationships

--When defining a recursive function, to find a base case, ask 
--"For which cases does recurson not make sense?"






