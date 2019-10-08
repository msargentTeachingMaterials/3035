
--btw, if you want to import the module Data.List to use its sort method:
import Data.List

{-
*Lambdas*

Anonymous functions used only once
Syntax: (\ params -> function body)

Try:
ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
ghci> map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 

-}
-- our own flip
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f = \x y -> f y x 

{-
*folds*

The common recursion pattern of combining a transformed head 
(by some function or operation or other) to
a recursive call to the tail has been encapsulated into the 
fold function

Fold takes a function that takes two parameters, an initial value 
(called the accumulator), and a list.

For left folds (foldl):

It calls that function on the accumulator and the head
of the list, producing a new value, updating the accumulator with
the new value.

Then it repeats the same thing with the same function, the accumulator
as the new value, and tail of the list passed in the time before.

It will keep doing this until the list is traversed, producing a 
single value.

e.g.
foldl (+) 0 [1,2,3] takes 0 and adds it to 1, then calls
foldl (+) 1 [2,3], which takes 1 and adds it to 2, producing
a new accumulator 3, then calls
foldl (+) 3 [3], which takes 3 and adds it to 3 and calls
foldl (+) 6 [], which returns 6
-}

--A more verbose way to code sum using lambdas

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs  

-- another implementation of elem using left fold
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys 
-- see board for how this works out

{-
Right fold (foldr): does the same thing, except works from the right end of
the list to the left

Use right folds when building up new lists from a list.
-}

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs  


-- foldl1 and foldr1 are just like the folds above, except no accumulator is entered,
-- they assume the first accumulator is the first or last element of the list

-- For illustration, here's some standard library functions implemented with folds:
maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
  
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x) 

{-
*scan*
Does the same thing as fold, but reports intermediate values in the form of a list

Try:
ghci> scanl (+) 0 [3,5,2,1]  
ghci> scanr (+) 0 [3,5,2,1]  
ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]  
ghci> scanl (flip (:)) [] [3,2,1]  



* $ *

$ is a convenience function that just calls a function for you with the parameter following the
$. It's implemented 
with:

($) :: (a -> b) -> a -> b  
f $ x = f x  

you pass in a function f, then $, then a params x, y, ..., and it gives you f x y , etc.

Its purpose is that it's low priority, allowing the elimination of parentheses
Basically it says to apply f on the left only after everything on the right is 
finished: it's right-associative

Try:
ghci> sum $ map sqrt [1..130]

Notice no parentheses are needed, without $ we would need
ghci> sum (map sqrt [1..130])
if we wanted sum to occur after the mapping occured

Compare:
ghci> sqrt $ 3 + 4 + 9
and 
ghci> sqrt 3 + 4 + 9
and 
ghci> sqrt (3 + 4 + 9)


Also, consider:
ghci> map ($ 3) [(4+), (10*), (^2), sqrt]

This involves a partial application of $, where the function is what is missing (but
the parameter is provided), and the list is a list of functions. map applies the param
to all the functions in the list.


*Function Composition*

f . g means apply g, then apply f to the result of applying g, to some parameter

Here's how it is defined:

(.) :: (b -> c) -> (a -> b) -> a -> c  
f . g = \x -> f (g x)  

f . g returns a new function that takes in the parameter g takes in, applies g to them,
then applies f to the result of that

Consider: 
Prelude> let neg3 = negate . (* 3)
Prelude> neg3 4
Prelude> (negate . (* 3)) 9 
Prelude> (negate . abs) (-9)
Prelude> ((^2) . abs) (-9)

Also consider:
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]  
versus
ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]  

Compare:
ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
versus

ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]] 

What about multiple parameters?

sum (replicate 5 (max 6.7 8.9))
versus
(sum . replicate 5 . max 6.7) 8.9 
versus
sum . replicate 5 . max 6.7 $ 8.9 


* Point Free Style * 

Instead of including parameters when defining functions, we can often define
funtions without mentioning them: that's point free style

Consider:

sum' :: (Num a) => [a] -> a     
sum' xs = foldl (+) 0 xs     
versus


fn x = ceiling (negate (tan (cos (max 50 x))))  
versus
fn = ceiling . negate . tan . cos . max 50  

Some useful functions for indices
findIndex and findIndices give indices that pass the filter
requires import Data.List (put at the top of your file)
-}

findit = findIndices (>3) [0,2,4,6,8]
