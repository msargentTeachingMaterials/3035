{-
* Custom typeclasses* 
How to make our own typeclasses and 
how to make types instances of them by hand

Here is a way to define the typeclass Eq:

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool  
    x == y = not (x /= y)  
    x /= y = not (x == y) 

The syntax is to use the class keyword, then the name of the 
typeclass, then a type variable (when instances are created,
this gets replaced with an actual type, then where, then 
type declarations of the functions the type is supposed to implement

Think of the above as somewhat analogous to defining an interface
in Java, except you can also implement functions (though this is 
optional)

* Manually making a type an instance of a type class *

Suppose we wanted to make a new type, TrafficLight as follows:
-}

data TrafficLight = Red | Yellow | Green  

{-
Suppose also we want it to implement Eq. We could, like before write:

data TrafficLight = Red | Yellow | Green deriving (Eq)

What if we want more control
over how the functions used in Eq work for TrafficLight. Then we can
write:
-}

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    Yellow == Red = True
    _ == _ = False  

instance Show TrafficLight where  
    show Red = "Not Yellow or Green"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

{-
If we do it this way, we can define the functions how we want. Here
We define == for each of the combinations we can get with 
TrafficLight

Note: in the definition of Eq, we defined == in terms of /= and vice
versa. If we didn't do this, we would also have to implement that in 
our TrafficLight instance

Try:
ghci> Red == Red  
True  
ghci> Red == Yellow  
False  
ghci> Red `elem` [Red, Yellow, Green]  
True  
ghci> [Red, Yellow, Green]  
[Red light,Yellow light,Green light]  


Typeclasses can also be of other type classes
We could

class (Eq a) => Num a where  
. . .

This would be a definition of Num that requires a to first be 
an Eq


-}