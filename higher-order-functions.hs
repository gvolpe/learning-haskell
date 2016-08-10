-- could be written as multThree :: (Num a) => a -> (a -> (a -> a))
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z 

-- partially applied functions
multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 2

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
-- same thing as: compareWithHundred x = compare 100 x
compareWithHundred = compare 100

-- infix functions partially applied
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) 

isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  

-- order matters sometimes. TRY applyTwice (*2) 3 OR applyTwice (multThree 2 2) 9
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

-- getting interesting. TRY zipWith (*) [1,2,3] [4,5,9] OR zipWith' max [6,3,2,1] [7,3,1,5] OR zipWith' (*) (replicate 5 2) [1..]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

-- TRY flip' zip [1,2,3,4,5] "hello"
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y 

-- not yet functors, only lists. TRY map (+3) [1,5,3,1,6] OR map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- NOTE: map (+3) [1,5,3,1,6] is equal to [x+3 | x <- [1,5,3,1,6]]
map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs  

-- filter takes a predicate (a -> Bool) and a list. TRY filter' (>3) [1,2,6,3] OR filter even [1..10]
filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs

-- quick sort algorithm using filter
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted  

-- largest divisible by 3829
largestDivisible :: (Integral a) => a  
largestDivisible = head (filter p [100000,99999..])  
    where p x = x `mod` 3829 == 0

-- same as this using lists comprehensions: sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
allOddSquaresSmallerThanTenThousand = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

-- Collatz sequences
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1) 

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

-- partially applied map
listOfFuns = map (*) [0..]  
aa = (listOfFuns !! 4) 5

----------- lambdas \ (or anonymous functions)

bb = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  

-- defined using lambdas = numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- pattern matching in lambdas
cc = map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- same thing
addThree :: (Num a) => a -> a -> a -> a  
-- addThree x y z = x + y + z -- normal way
addThree = \x -> \y -> \z -> x + y + z  -- lambdas here

flipLambda :: (a -> b -> c) -> b -> a -> c  
flipLambda f = \x y -> f y x  

----------- folds

-- using foldl (fold left)
sum' :: (Num a) => [a] -> a  
--sum' xs = foldl (\acc x -> acc + x) 0 xs  
sum' = foldl (+) 0 -- cleaner

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

-- map using fold right
mapF :: (a -> b) -> [a] -> [b]  
mapF f xs = foldr (\x acc -> f x : acc) [] xs 

-- using foldl1 or foldr1. NOTE: Doesn't work with empty lists!!!
sum2 = foldl1 (+)

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  

-- same as: foldl (flip (:)) [] 
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filterF :: (a -> Bool) -> [a] -> [a]  
filterF p = foldr (\x acc -> if p x then x : acc else acc) []  
  
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)

-------------- scanners

s1 = scanl (+) 0 [3,5,2,1]
s2 = scanr (+) 0 [3,5,2,1]
s3 = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
s4 = scanl (flip (:)) [] [3,2,1]

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

gg = sum (map sqrt [1..131])

-------------- function application with $

-- the function after $ is applied as the parameter of sum
hh = sum $ map sqrt [1..130]

-- this two expressions generate the same result
-- sqrt $ 3 + 4 + 9 == sqrt (3 + 4 + 9)
-- different from this!!! sqrt 3 + 4 + 9

-- using $ the parameter will be treated as a function hence this is possible
ii = map ($ 3) [(4+), (10*), (^2), sqrt]

-------------- function composition

negation = negate . (* 3)

negatives = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24] 
negativesCompose = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- f (g (z x)) is equivalent to (f . g . z) x

nst = map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
nstCompose = map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- same thing using composition
k1 = sum (replicate 5 (max 6.7 8.9))
k2 = sum . replicate 5 . max 6.7 $ 8.9 -- apply 8.9 to the function composed on the left side

z1 = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
z2 = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8] -- same as above

fn x = ceiling (negate (tan (cos (max 50 x))))  
fnCompose = ceiling . negate . tan . cos . max 50

-- different ways to write the same thing
oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSumCompose :: Integer  
oddSquareSumCompose = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSumLet :: Integer  
oddSquareSumLet =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit
