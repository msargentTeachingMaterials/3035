import Data.List  
  
main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice   


{- 
Heathrow to London


S --- 50 --- A1 --- 5 --- A2 --- 40 --- A3 --- 10 --- G
             |            |             |             |    
       	     30           20            25            0
             |            |             |             |          	                    
S --- 10 --- B1 --- 90 --- B2 --- 2 --- B3 --- 8  --- G

How to find the shortest path and distance from S to G, manually?

Find the shortest paths to A1 and B1 (one for each), storing the distance traveled
to each, then make them be the new S's, 

Then, from the new S's (which have a starting amount of distance traveled), find 
the shortest paths to A2 and B2, and make them the new S's, and so on, until you reach a G.

Implementing this in Haskell
-}

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]  

{-
Each section contains the distance of the road from the current A to the next A, 
current B to the next B, and the distance from the next A to the Next B, so the
above map would have sections with the values below:

50, 10, 30 
5, 90, 20 
40, 2, 25
10, 8, 0

A RoadSystem is just a list of Sections, in order from left to right.
-}

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

{-
To represent a solution, we are going to use a list that specifies a move and a distance
Label will represent the move, Path represents a list of tuples for the move and the 
distance of the move
-}

data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  

{-
What a solution would look like:
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]

The solution will be to walk from left to right over the map of sections, accumulating
a shortest path as we go: this is a job for foldl.
-}

roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  


--The folding operation:

optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath  

{-
The last lines are because our path is reversed, since we are adding at the begging
when we build the list because that's more efficient than adding at the end

Last part: getting a text file with distances listed, where the first 3 define a 
section, the second three define a section, and so on:

50  
10  
30  
5  
90  
20  
40  
2  
25  
10  
8  
0 


We need to convert them to sections, then
output the shortest path

Grouping the input into sections of n
-}

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs) 

