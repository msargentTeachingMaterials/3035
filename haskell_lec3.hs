{-
**Types**
*Intro*
- Haskell is statically typed and has type inference
- To find the type of an expression, use
:t

Try :
Prelude>:t 'a'
Prelude>:t (True, 'a')
Prelude>:t 4 == 5

"::" means "has type of"

Try:
Prelude>:t "test"
[Char] means a list of characters

- Function type declarations: It's a good practice 
to declare types of functions

- type the name of the function, ::, then the type going in, 
->, then the type going out
After this, type the function definition like we did before

Prelude>removeNonUppercase :: [Char] -> [Char]
Prelude>removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]  

- for functions with several parameters

Prelude>addThree :: Int -> Int -> Int -> Int 
Prelude>addThree x y z = x + y + z

- The arrows are just separators, the last element in the
type declaration is the return type, all the others are 
formal parameters

- When in doubt about which types to use, try defining your function
without the type declaration and see what types Haskell gives you
you when you use :t on it

Try:
Prelude>:t addThree

*Some different types*

- Int -- same range as Java int, 32 bits 
- Integer -- unlimited length

- e.g.:
factorial :: Integer -> Integer
factorial n = pruduct [1 .. n]

- Float -- single precision floating point (32 bits)
- Double -- double precision floating point (64 bits)

Try:
Prelude>circumference :: Float -> Float  
Prelude>circumference r = 2 * pi * r  

Prelude>circumference' :: Double -> Double  
Prelude>circumference' r = 2 * pi * r  

Prelude>circumference 4.0 
Prelude>circumference' 4.0 

- Bool -- True or False
- Char -- a character, denoted by single quotes
a list of Char's is a String 

*Type Variables*

Try:
Prelude>:t head

- you should get:
head :: [a] -> a


- All types have to begin with upper case, but 'a'
is lower case. They are type variables. They are like 
Java generics type variables

- head is a polymorphic function

Try:
Prelude>:t fst

- It's type will be a tuple of two types (may be the same) and 
returns a value of the first type in the tuple
That is, it will be: fst :: (a, b) -> a 

*Typeclasses*
- Not like Java classes, more like interfaces
- Their members are in classes that meet the requirements
of the interface

Try:
Prelude>:t (==)

(==) :: (Eq a) => a -> a -> Bool  

=> is a class constraint
- Read the above as:
"==" is defined as --
"Take any two values that are members of the Eq class and return
a boolean"

- Some basic typeclasses:
- Eq -- things that can be equal or not, it's definition includes 
== or /= (not equals)
- Ord -- ordered types, that can be compared
compare takes two Ord values
- All Ord's are Eq's
- Show -- these values can be represented by strings
the function "show" takes a Show

Try Prelude>show 6

- Read -- the opposite of Show, strings that can be read as a
number, Bool, etc. Can be passed into the function "read"

Try Prelude>read "456" + 7

- If there is an ambiguity about which type to return, 
an error will occur

Try Prelude>read "4"

- See the type signature of read

Try Prelude>:t read

- The return type is a generic

- Type annotations can fix this: a way to explicity determine
the type to return

Try:

Prelude> read "5" :: Int   
Prelude> read "5" :: Float  
Prelude> (read "5" :: Float) * 4  
Prelude> read "[1,2,3,4]" :: [Int]  
Prelude> read "(3, 'a')" :: (Int, Char)  

- Enum -- sequentially ordered types, enumerable
these work with cucc and pred (predecessor)

Try:

Prelude> ['a'..'e']  
"abcde"  
Prelude> [LT .. GT]  
[LT,EQ,GT]  
Prelude> [3 .. 5]  
[3,4,5]  
Prelude> succ 'B'  
'C'  


- Bounded -- must have an upper or lower bound, can call
minBound and maxBound on these

Try:

Prelude> minBound :: Int  
-2147483648  
Prelude> maxBound :: Char  
'\1114111'  
Prelude> maxBound :: Bool  
True  
Prelude> minBound :: Bool  
False 

- Num -- numeric typeclass, can act like numbers

Try:

Prelude> :t 20  
20 :: (Num t) => t  

int, Integer, Float, and Double are Num's

Prelude> 20 :: Int  
20  
Prelude> 20 :: Integer  
20  
Prelude> 20 :: Float  
20.0  
Prelude> 20 :: Double  
20.0  

- Some functions take any Num

Try:
Prelude> :t (*)  
(*) :: (Num a) => a -> a -> a  

- Integral -- includes only whole numbers
- Floating -- only floating point numbers like Float
and Double
- The function fromIntegral can convert an Integral to
an Num, good for making Ints play nice with Floats

-}