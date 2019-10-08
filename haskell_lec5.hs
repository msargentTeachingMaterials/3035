{-
*Guards*

Guards are an alternative to if statements. See bmi
function below for syntax example. "othewise" is a 
catchall often found at the end
-}
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight"  
    | bmi <= 25.0 = "You're normal"  
    | bmi <= 30.0 = "You're overweight"  
    | otherwise   = "You're obese"  


-- with two parameters
weightHeightTell :: (RealFloat a) => a -> a -> String  
weightHeightTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal"  
    | weight / height ^ 2 <= 30.0 = "You're overweight"  
    | otherwise                   = "You're obese"  

-- note: no "=" before the first guard

-- a max function
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  

{-
*where*

if you want to use the result of some calculations, assignments, 
etc, in all your guards, use a where clause after the guards, as
in the example below
-}

bmiTell1 :: (RealFloat a) => a -> a -> String
bmiTell1 weight height  
    | bmi <= skinny = "You're underweight"  
    | bmi <= normal = "You're supposedly normal" 
    | bmi <= fat    = "You're overweight"   
    | otherwise     = "You're obese"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  

{- 
note that names defined with where are only locally visible
make sure the names are aligned into a single column
if you make fcns w several patterns, the where isn't shared 

you can use where to pattern match
-}
bmiTell2 :: (RealFloat a) => a -> a -> String  
bmiTell2 weight height  
    | bmi <= skinny = "You're underweight"   
    | bmi <= normal = "You're supposedly normal"  
    | bmi <= fat    = "You're overweight"   
    | otherwise     = "You're obese"   
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 20.0, 30.0)



-- consider this function
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  

-- This function takes in first and last name and returns initials
-- "where" bindings can be used here as well (though not as simple
-- as just using pattern matching in the parameters)

-- you can also define a function in the where clause, like so

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2

{-
*let . . . in*
if you want a guard-specific binding, use let instead of where
syntax: let<bindings> in <expression>
put the binding before the expression (opposite of where)-}

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^ 2  
    in  sideArea + 2 * topArea  

{- 
let bindings are actual expressions, and not syntactic constructs
That means they can be used anywhere expressions can be used


Try this: 
if ... then ... else ...
ghci> 4 * (if 10 > 5 then 10 else 0) + 2 
let ... in ... 
ghci> 4 * (let a = 9 in a + 1) + 2  

You can use function definitions in your let expressions so 
that they have only local scope
ghci> [let square x = x * x in (square 5, square 3, square 2)]  

If you want to have several variables inline, use semicolons to 
separate the values

ghci> (let a=100; b=200; c=300 in a*b*c, let foo="Hey "; bar="there!" in foo ++ bar)

You can pattern match with let bindings. This is useful for separating
parts of a tuple into components and binding them to names and such

ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100  

-}

{-
Let bndings can also be used in list comprehensions
Revisiting calcBMIs
-}

-- the let expression is written just like a predicate
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]  


-- using let in a list comprehension along with actual predicates
calcBmisf :: (RealFloat a) => [(a, a)] -> [a]  
calcBmisf xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 


{-
Sometimes the in can be omitted when you want the scope to be the
entire interactive session, or in a list comprehension when the 
scope is the whole comprehension

Try
ghci> let zoot x y z = x * y + z 

Then try
ghci> zoot 3 9 2  


*Case Expressions*

Syntactic sugar for pattern matching expressions

Syntax:
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  

The following code snippets are interchangable (except that
they have different names)
-}

head' :: [a] -> a
head' [] = error "No head for empty lists"
head' (x:_) = x

head2 :: [a] -> a  
head2 xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  

{-
Case expressions can be used outside of defining functions (unlike
pattern matching)

In this case, the case expressions aren't used to define the function
as to add something to an already existing function definition
-}

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

                         








