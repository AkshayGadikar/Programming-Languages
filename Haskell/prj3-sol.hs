import Data.List
import Data.Maybe
import Debug.Trace

-- Problem 1
-- Return pair containing roots of quadratic equation a*x**2 + b*x + c.
-- The first element in the returned pair should use the positive 
-- square-root of the discriminant, the second element should use the 
-- negative square-root of the discriminant.  Need not handle complex
-- roots.

-- a b c provide roots. Ord used for comparision operator
quadraticRoots :: (Floating t,Ord t) => t -> t -> t -> (t, t)
quadraticRoots a b c = if term1 < 0 then error "0" else (x, y)
                          where
                            x = (term2 + sqrt term1) / den
                            y = (term2 - sqrt term1) / den
                            term1 = b * b - 4 * a * c
                            term2 = - b
                            den = (2 * a)
                          
                          
-- Problem 2
-- Return infinite list containing [z, f(z), f(f(z)), f(f(f(z))), ...]
-- May use recursion.

--iterate function used to iterate list 
iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f z = iterate f z

-- Problem 3
-- Using iterateFunction return infinite list containing 
-- multiples of n by all the non-negative integers.
-- May NOT use recursion.

-- apply function to every element of list using iterating the list.
multiples n = map (\x -> x * n) (iterateFunction (\x->x+1) 0) 
  

-- Problem 4
-- Use iterateFunction to return an infinite list containing list 
-- of hailstone numbers starting with n.
-- Specifically, if i is a hailstone number, and i is even, then
-- the next hailstone number is i divided by 2; if i is a hailstone
-- number and i is odd, then the next hailstone number is 3*i + 1.
-- May NOT use recursion.

--if even : num/2
--if odd: 3 * num +1
hailstones :: Integral a => a -> [a]
hailstones n = iterateFunction hailstonelist n
                where 
                    hailstonelist n = if even n then n `div` 2 else 3 * n + 1
                
                
-- Problem 5
-- Return length of hailstone sequence starting with n terminating
-- at the first 1.
-- May NOT use recursion.  Can use elemIndex from Data.List

-- length using elemindex + 1 to include 1 in list
hailstonesLen :: Integral a => a -> Int
hailstonesLen n = if n == 0 then 0 else 1 + fromJust(hail)
                    where 
                     hail = elemIndex 1 (hailstones n)
                   
              
                              
-- Problem 6
-- Given a list of numbers, return sum of the absolute difference
-- between consecutive elements of the list.
-- May NOT use recursion.

--sum function to provide sum of list returned
sumAbsDiffs :: Num a => [a] -> a
sumAbsDiffs numberList = sum [ abs (a-b) | (a:b:numberList) <- tails numberList ]
                    

-- Problem 7
-- The x-y coordinate of a point is represented using the pair (x, y).
-- Return the list containing the distance of each point in list
-- points from point pt.
-- May NOT use recursion.

--dist = sqrt[(x1-x2)^2 + (y1-y2)^2]
distances :: Floating b => (b, b) -> [(b, b)] -> [b]
distances (x1,y1) [] = []
distances (x1,y1) points = map (distance (x1,y1)) points
   where  distance :: Floating b => (b, b) ->(b,b) -> b
          distance (a,x) (c,d) = sqrt ((a - c)^2 + (x - d)^2)

-- Problem 8
-- Given a list of coordinate pairs representing points, return the
-- sum of the lengths of all line segments between successive 
-- adjacent points.
-- May NOT use recursion.

--sum function to return sum of list
sumLengths :: Floating a => [(a, a)] -> a
sumLengths pointsList = sum [distance (x1,y1)(x2,y2) |((x1,y1):(x2,y2):pointsList) <- tails pointsList]
                         where distance (a,x) (c,d) = sqrt ((a - c)^2 + (x - d)^2)

                                           
-- Problem 9
-- Given a string s and char c, return list of indexes in s where c
-- occurs
occurrences s c = c `elemIndices` s
  

-- A tree of some type t is either a Leaf containing a value of type t,
-- or it is an internal node (with constructor Tree) with some left
-- sub-tree, a value of type t and a right sub-tree.
data Tree t = Leaf t
            | Tree (Tree t) t (Tree t)

-- Problem 10
-- Fold tree to a single value.  If tree is a Tree node, then it's
-- folded value is the result of applying ternary treeFn to the result
-- of folding the left sub-tree, the value stored in the Tree node and
-- the result of folding the right sub-tree; if it is a Leaf node,
-- then the result of the fold is the result of applying the unary
-- leafFn to the value stored within the Leaf node.
-- May use recursion.
foldTree :: (t1 -> t -> t1 -> t1) -> (t -> t1) -> Tree t -> t1
foldTree treeFn leafFn tree = 
    case tree of
        (Leaf t) -> leafFn t
        (Tree left root right) -> treeFn (foldTree treeFn leafFn left) root (foldTree treeFn leafFn right )
     
  
-- Problem 11
-- Return list containing flattening of tree.  The elements of the
-- list correspond to the elements stored in the tree ordered as per 
-- an in-order traversal of the tree. Must be implemented using foldTree.
-- May NOT use recursion.
flattenTree :: Tree a -> [a]
flattenTree tree = foldTree (\t1 t t2->t1 ++ [t] ++ t2) (\x->[x]) tree


-- Problem 12
-- Given tree of type (Tree [t]) return list which is concatenation
-- of all lists in tree.
-- Must be implemented using flattenTree.
-- May NOT use recursion.
catenateTreeLists :: Tree [a] -> [a]
catenateTreeLists tree = foldr (++) [] $flattenTree tree



