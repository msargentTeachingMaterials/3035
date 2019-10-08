* Monoids *
See https://wiki.haskell.org/Monoid
http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html

Monoid is a type class that requires this of its types:
1. A rule that takes any two instances of the type to create
another instance of the type
2. A value that's equivalent to nothing, so that when added to an
instance of the type just returns the instance unchanged

Example: lists have rules to combine two lists to create a list, and 
for combining [] with a list to leave the list unchanged

Here's the class definition:

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  -- defining mconcat is optional, since it has the following default:
  mconcat = foldr mappend mempty

Together with these laws (not enforced, but expected):
-- Identity laws
x <> mempty = x
mempty <> x = x
 
-- Associativity
(x <> y) <> z = x <> (y <> z)


We could manually make list an instance of Monoid:
instance Monoid [a] where
    mappend = (++)
    mempty = []

Why Monoids?

Free function: monocat --- combines elements of a list into one
element (like fold). This function is implemented by default
as shown above.