{-
-List Comprehensions: modeled after set comprehensions 
in mathematics

[something you want to do with x | for every x in some list, 
that meets a criterion]

Try at the prompt:
let a = [1 .. 100]
let b = [x*2 + 5 | x <- a]
let c = [x*2 + 5 | x <- a, x `mod` 7 == 0]
let d = [x**2 | x <- [0,5 .. 100]]

- before the pipe: output function
- x is the variable
- x `mod` 7 == 0 is the predicate
- a is the input set
- predicates are used for filtering: removing elements 
we don't want
- we can use as many as we want, separated by commas

- we can write more complex output functions using conditional 
expressions

-}

boomBangs alist = [if even x then "BOOM!" else "BANG!" 
                    | x <- alist]

-- Here's fizzbuzz

fizzbuzz a b = [if x `mod` 15 == 0 then "FIZZBUZZ!" 
    else if x `mod` 3 == 0 then "FIZZ!" 
    else if x `mod` 5 == 0 then "BUZZ!" else show x
    | x <- [a .. b]]

{-
- Multiple predicates can be used as well:
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  

- You can use multiple lists:
[ x*y | x <- [2,5,10], y <- [8,10,11]]  

- Try at prompt:
let nouns = ["vagrant", "pizza boy", "pokemon"]
let adjectives = ["over-achieving", "smarmy", "gauche", "reprehensable"]
[adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]  

-}

-- our own length function
length' l = sum[1 | _ <- l]
-- underscore is for a variable we don't care about: all we want is a 1
-- for every element in the list, then we sum for the length

-- removing lowercase from a string
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

{-
Tuples:
- A Tuple, like a list, stores multiple values
- The syntax is the same as lists with parentheses replacing brackets
- Tuples of different lengths are different types
(1,2) is a different type than (1,2,3), but the same type as (3,4)
- Tuples need not be homogeneous
- There are no singleton tuples

- Useful if you want to enforce the number of values in a data structure
    e.g., if you want to prevent jagged matrices
        Can't use a list of lists, for they can be different sizes
        Use list of tuples
        [(1,2),(8,11),(4,5)] is legal
        [(1,2),(8,11,5),(4,5)] will generate an error

- Built-in functions for tuples
fst: gets first of a two item tuple
fst (8, 12)

snd: gets last item of two item tuple
snd (8, 11)

The above two functions only work on 2 item tuples

zip takes two lists and combines them, element by element,
into a list of tuples
zip [1,2,3,4,5] [5,5,5,5,5] produces
[(1,5),(2,5),(3,5),(4,5),(5,5)] 

and

zip [1 .. 5] ["one", "two", "three", "four", "five"] produces 
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]   
With two lists of different lengths, the longer list is cut off 
to match the shorter one


- Problem: find all right triangles with sides of length (integers) less
than 10 with a perimeter of 24

Also, a <= b, b <= c, 

First, get all triples of sides less than 10

let triples = [(a,b,c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b]]

Then get right triangles

let rightTriangles = [(a,b,c) | (a, b, c) <- triples, c^2 == b^2 + a^2]

let ourTriangles = [(a,b,c) | (a, b, c) <- rightTriangles, a+b+c==24]

Here's the one-liner
let rightTriangles' = [ (a,b,c) 
| c <- [1..10], b <- [1..c], 
a <- [1..b], 
a^2 + b^2 == c^2, a+b+c == 24] 
-}