-- import all the functions in the module Data.List
import Data.List  
import Data.Function (on)

numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub 

---------- selective imports

------ only nub and sort
-- import Data.List (nub, sort)

------ exclude nub
-- import Data.List hiding (nub)

-- to call filter you would use M.filter
-- import qualified Data.Map as M

---------- functinos in Data.List

-- takes element and a list
a1 = intersperse '.' "MONKEY"  
a2 = intersperse 0 [1,2,3]

-- takes a list and a list of lists and flattens the result
b1 = intercalate " " ["hey","there","guys"]  
b2 = intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]] 

-- like a 2D matrix
c1 = transpose [[1,2,3],[4,5,6],[7,8,9]]

-- flattens a list of lists
d1 = concat ["foo","bar","car"]

-- map a function and then concat
e1 = concatMap (replicate 4) [1..3]

-- and takes a list of booleans and evaluates them
f1 = and $ map (>4) [5,6,7,8]  
f2 = and $ map (==4) [4,4,4,3,4]

-- same with or
g1 = or $ map (==4) [2,3,4,5,6,1]  
g2 = or $ map (>4) [1,2,3]  

-- any & all take a predicate and a list
h1 = any (==4) [2,3,5,6,1,4]  
h2 = all (>4) [6,9,10]  
h3 = all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
h4 = any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"

-- takes a function and a starting value
i1 = take 10 $ iterate (*2) 1

-- takes an index and a list
j1 = splitAt 3 "heyman"

-- takes a predicate and a list
k1 = takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]

-- filter wouldn't work here (infinite list)
l1 = sum $ takeWhile (<10000) $ map (^3) [1..]

-- same as takeWhile
m1 = dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]

stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
stockExceedingOneThousand = head (dropWhile (\(val,y,m,d) -> val < 1000) stock)

-- span is like takeWhile but also returns an additional list with the values that would have been dropped
n1 = span (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]

-- same as negate span
o1 = break (==4) [1,2,3,4,5,6,7] -- same as: span (/=4) [1,2,3,4,5,6,7]

-- elems have to be part of Ord typeclass
p1 = sort [8,5,3,2,1,6,4,2]

q1 = group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

-- how many times an element appears in a list?
appearances = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  

-- applying init and tail recursively
r1 = inits "w00t"
r2 = tails "w00t"  
--let w = "w00t" in zip (inits w) (tails w)

-- search in sublists
s1 = "cat" `isInfixOf` "im a cat burglar"
s2 = "dog" `isInfixOf` "im a cat burglar"
s3 = "Cat" `isInfixOf` "im a cat burglar"

s4 = "hey" `isPrefixOf` "hey there!"
s5 = "hey" `isSuffixOf` "hey there!"

-- similar to span and break but goes through the whole list
t1 = partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" --Try this to see the difference: span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"

-- find the first element that satisfies the predicate (like in Scala) returns a Maybe 
u1 = find (>4) [1,2,3,4,5,6]  
u2 = find (>9) [1,2,3,4,5,6]

-- returns a Maybe with the index
v1 = 4 `elemIndex` [1,2,3,4,5,6]

-- returns a list of indices
v2 = ' ' `elemIndices` "Where are the spaces?"

-- returns Maybe the first index that satisfies the predicate
v3 = findIndex (==4) [5,3,2,1,6,4]  
v4 = findIndex (==7) [5,3,2,1,6,4]

-- more zips
w1 = zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
w2 = zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]

-- lines and unlines
x1 = unlines ["first line", "second line", "third line"]
x2 = lines x1

-- words and unwords
x3 = unwords ["hey","there","mate"]
x4 = words x3

-- takes a list and weeds out the duplicate elements
y1 = nub [1,2,3,7,4,2,4]

-- deletes the first occurence
z1 = delete 'h' "hey there ghang!"

-- list difference function
aa = [1..10] \\ [2,5,9]

-- like a function on sets
bb = [1..7] `union` [5..10]

-- intersection
cc = [1..7] `intersect` [5..10]

-- insert sorted
dd = insert 4 [3,5,1,2,8,2]
dd1 = insert 4 [1,2,3,5,6,7]

-- genericLength, take, drop, splitAt, index, replicate
--let xs = [1..6] in sum xs / genericLength xs

-- functions that take an equality function to compare elements instead of using ==:
-- nubBy, deleteBy, unionBy, intersectBy and groupBy

values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
grouped = groupBy (\x y -> (x > 0) == (y > 0)) values

-- using Data.Function (on)
groupedClearer = groupBy ((==) `on` (> 0)) values

-- more functions
-- sortBy, insertBy, maximumBy, minimumBy

xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
sortedListByLength = sortBy (compare `on` length) xs
