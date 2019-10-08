{-
**Syntax in Functions**
*Pattern Matching*

We can have functions do different things, depending on 
what goes in.

We define several versions of the function with different
patterns for the domain. Haskell figures out which
pattern fits best.

It checks the patterns in order, and the first one 
that fits is used,
-}

-- consider
lucky :: (Integral a) => a -> String
lucky 7 = "You won $100,000 at craps!"
lucky x = "You lost everything. To continue, go to Auto Title Loans."

-- also consider
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

-- consider this recursive function of factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- failure of pattern matching
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  
{- if you put in something not listed here it 
crashes  complaining of non-exhaustive patterns-}

-- using it on tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- define functions to extract members of triples
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

{-lists can be used in pattern matching
fyi: implentation note: [1,2,3] is equivalent to
1:2:3:[], as it's a linked list

a common pattern x:xs  - works for lists with at 
least 1 element, x is the first element, xs is the 
rest of the list

How do you bind the first 3 elements to variables 
and the rest of the list to another variable? Try
x:y:z:zs
-}

-- our own implementation of head
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list"  
head' (x:_) = x 

-- note that using more than one varable requires 
-- parentheses

-- here's a function that tells you some of the 
-- beginning elements are

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- here's a length function using patterns and recursion
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:the_rest) = 1 + length' the_rest  

-- sum with patterns and recursion
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- binding variables to patterns: v@pattern to avoid 
-- repeating
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 

-- one thing, you can't use ++ in pattern matches
