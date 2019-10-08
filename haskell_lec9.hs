import Data.List

{-
** Modules **
These are collections of related functions, 
and typeclasses

They aren't automatically available: you have to import them

in a script, put this at the top:
import <module name>
example:
import Data.List

At the command prompt:
prelude>:m Data.List

If you want to avoid namespace collisions, use qualified
import qualified (have to type module name)
import qualified Data.List

to use:
Data.List.sort

or use an alias in the import:

import qualified Data.List as dl

Learn You a Haskell has descriptions of a bunch of modules: look them
over.

** Custom Data Types**

Define a new data type, Shape

-}

--data Shape = Circle Float Float Float | Rectangle Float Float Float Float

{-
The first word is data, then the name of the type (capitalized, then =, 
then the constructors separated by pipes)

Note, Circle and Rectangle are not subtypes of Shape, they are functions that 
serve as constructors for Shape

Try:

ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape  

A function that takes a Shape and returns a Float that represents the surface area
of the Shape
-}

surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  

{-
Using it:

ghci> surface $ Circle 10 20 10  
314.15927  
ghci> surface $ Rectangle 0 0 100 100  
10000.0  

Can't print a Circle, because it's not a Show. But we can fix that with
(comment in, comment out earlier one)
-}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
 {-
Then:
ghci> Circle 10 20 5  
Circle 10.0 20.0 5.0  
ghci> Rectangle 50 230 60 90  
Rectangle 50.0 230.0 60.0 90.0  

Since constructors are functions, we can partially apply them

ghci> 
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0] 
creating a concentric ring of circles

Using a derived type (Point) to help define another (Shape) for more clarity:

data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  


Then we can define a clearer version of the surface function

surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  


*Record syntax*

A clearer syntax for new data types using this syntax
 -}
data Person = Person {firstName :: String,
 lastName :: String, 
 age :: Int, 
 height :: Float, 
 phoneNumber :: String, 
 flavor :: String} deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving(Show)  


{-
Easier when there are a lot of different params and we'd like to keep track of
their names. Each "name" is a function, of course

ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String  

Using this syntax causes prints of records to be clearer and have more information.

ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967} 

Type parameters
:t
consider this built in data type:

data Maybe a = Nothing | Just a  

This is useful if you might return Nothing (Haskellese for null) --- remember null 
checks?
Using a type parameter allows any type to be used in Maybe, and no type can be 
just a Maybe. 

Instead you have:

Maybe Char
Maybe String
Maybe Int and so on

ghci> Just "Haha"  
Just "Haha"  
ghci> Just 84  
Just 84  
ghci> :t Just "Haha"  
Just "Haha" :: Maybe [Char]  
ghci> :t Just 84  
Just 84 :: (Num t) => Maybe t  
ghci> :t Nothing  
Nothing :: Maybe a  
ghci> Just 10 :: Maybe Double  
Just 10.0  

list types also use type parameters --- there is no [] type, but there is [Char] or 
[Int]

Another type that uses type parameters is Data.Map. This type is a hash or a 
dictionary.

Map k v
has k type of keys, and v type of values, so we can have maps from any type to 
any other type

Note: never add typeclass constraints in data declarations (put them into functions 
instead)


A new type Vector
-}

data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  


{-
Here we have to repeat the typeclasses in the constructors anyway, so it's redundant


Using the constructors:

ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
Vector 12 7 16  
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
Vector 12 9 19  
ghci> Vector 3 9 7 `vectMult` 10  
Vector 30 90 70  
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
74.0  
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
Vector 148 666 222 


*Deriving Typeclasses*
Similar to implementing interfaces: says that this type supports the operations of 
the type classes, and those can be performed on this type.

-}

data Person' = Person' { 
firstName' :: String, 
lastName' :: String, 
age' :: Int} deriving (Eq, Show, Read)  

{-
*Recursive data structures*

Remember linked lists? Trees?

A type can include itself in its definition

Our own version of list:

data List a = Empty | Cons { listHead :: a, listTail :: List a} 
deriving (Show, Read, Eq, Ord)  

BTW, cons is the same as :

-}

data List' a = Empty | Cons { listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord) 

{-
Try:
ghci> Empty  
Empty  
ghci> 5 `Cons` Empty  
Cons 5 Empty  
ghci> 4 `Cons` (5 `Cons` Empty)  
Cons 4 (Cons 5 Empty)  
ghci> 3 `Cons` (4 `Cons` (5 `Cons` Empty))  
Cons 3 (Cons 4 (Cons 5 Empty)) 

*Binary search tree*
-- Review binary search trees
A tree is either empty, or it's an element that contains some value and two trees: a recursive
definition

-}
--The data type definition
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) 


-- Functions to insert elements into trees

-- Create a single Node
makeNode :: a -> Tree a  
makeNode x = Node x EmptyTree EmptyTree  

-- Inserting into a tree 
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = makeNode x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 


-- Check if an element is in a tree

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right   

{-
Populating a tree with a list

ghci> let nums = [8,6,4,1,7,3,5]  
ghci> let numsTree = foldr treeInsert EmptyTree nums  
ghci> numsTree  
Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))  

ghci> 8 `treeElem` numsTree  
True  
ghci> 100 `treeElem` numsTree  
False  
ghci> 1 `treeElem` numsTree  
True  
ghci> 10 `treeElem` numsTree  
False  

-}

