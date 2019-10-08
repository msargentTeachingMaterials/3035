{-
**Higher Order Functions**

Higher order functions involve either taking a
function in as a parameter or returning a function
as a result or both.

Haskell does all of the above.

*Curried functions*
- Every function in Haskell actually only takes 
one parameter
- Functions that "take" two or more parameters 
actually do something more complicated. For two 
parameters:
	* They take the first parameter and return 
	a function
	* Then the returned function is called with
	the second parameter passed in
	* This is somewhat reminiscent of closures 
	in JavaScript

For example:
max a b first returns a function
(max a): this function takes in one argument
and compares it to whatever got passed in before
for a, and returns the largest. Thus the 
type declaration of max could be written as
both of these:

max :: (Ord a) => a -> a -> a
max :: (Ord a) => a -> (a -> a)

Partially applied function: a function in which too
few parameters are entered, resulting in another
function rather than a result

The resulting function can then be called with the 
remaining parameters

Another example:
-}

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

{-
multThree 3 5 9 is the same as:
((multThree 3) 5) 9

The function's type could be:
multThree :: (Num a) => a -> (a -> (a -> a))


Try this: 
ghci> let multTwoWithNine = multThree 9
ghci> multTwoWithNine 2 3

ghci> let multWithEighteen = multTwoWithNine 2
ghci> multWithEighteen 

Consider the function below:
-}

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x 

compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100 

-- Both of these work the same, but the last returns
-- a function that is then called to make the comparison

-- for infix functions
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- (/10) returns a function that takes a Float and
-- divides it by 10: this is a section

-- a function that checks if a character is uppercase

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

{-
Sections and negatives: the negative sign is a 
function in haskell
Try: 
ghci> :t (-4)

It's a function that takes a number and subtracts
4 from it. But, for convenience, it just means 
negative 4. 

So, to clarify, if you want to make a function that 
subtracts 4 from the number it gets as a parameter, 
partially apply the subtract function like so: 
(subtract 4).

btw, if you try to partially apply a function at
the ghci prompt, (like multThree 4 5) you'll get an error, 
for ghci doesn't
know how to print a function to the screen

*Functions as parameters*

Here's a function that takes in a function as a parameter
and applies it twice
-}

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

{-
The (a -> a) part of the type declaration is a function
being being passed into applyTwice

The function takes whatever function you put in
and applies it, then applies it again on the result

Try:
ghci> applyTwice (+3) 10  
ghci> applyTwice (++ " Apply") "Apply"  
ghci> applyTwice ("Apply " ++) "Apply"  
ghci> applyTwice (multThree 2 2) 9  
ghci> applyTwice (3:) [1]  

To pass in a function, we can use partial application,
that is, when you don't put in all the parameters
of a function, it is partially applied, and the
result is another function, as in the last two
cases


Let's make our own implementation of zipWith.
zipWith takes two lists and a function that 
takes two parameters, and returns a list of 
the results of the function being applied to
pairs of elements from the two lists.
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

{-
Try at prompt:
zipWith' (+) [4,2,5,6] [2,6,2,3]   
zipWith' max [6,3,2,1] [7,3,1,5]  
zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]    
zipWith' (*) (replicate 5 2) [1..]   
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
-}

-- Here's an implementation of flip, which just switches the 
-- parameters of a two-parameter function

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  

{-
Try:
ghci> flip' zip [1,2,3,4,5] "hello"  
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  


*Maps and Filters*
map and filter are built in functions, and a core part
of functional programming

map takes a function and a list and returns a list of
the function applied to each element of the list
	- similar to a list comprehension

Here's how it is defined


map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs  


ghci> map (+3) [1,5,3,1,6]  
ghci> map (++ "!") ["BIFF", "BANG", "POW"]  
ghci> map (replicate 3) [3..6]  
ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]  
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]  


filter is a function that takes a predicate and a 
list and returns a list with all the elements for which
the predicate is true

Here's the implementation

filter :: (a -> Bool) -> [a] -> [a]  
filter _ [] = []  
filter p (x:xs)   
    | p x       = x : filter p xs  
    | otherwise = filter p xs  

at prompt:

filter (>3) [1,5,3,2,1,6,4,3,2,1]  
[5,6,4]  

filter (==3) [1,2,3,4,5]  
[3]  
filter even [1..10]  
[2,4,6,8,10]  
let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]  
[[1,2,3],[3,4,5],[2,2]]  
filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  
"uagameasadifeent"  

Both map and filter can be accomplished with list
comprehensions

Here's a slightly more readable version of quicksort
-}

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

-- Using filter to find the largest number under 100,000
-- that's divisible by 3829

largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [99999, 99998..])  
    where p x = x `mod` 3829 == 0  

{-
takeWhile takes a predicate and a list and goes through
the list, and puts the element into a new list if the predicate
is true. If it comes to an element in which the predicate 
is not true, the function stops and returns the new list. 

e.g., takeWhile (/=' ') "elephants know how to party" would
return "elephants"

Try:
ghci> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))  
166650  
-}


