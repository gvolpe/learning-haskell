-- using guards, where binding and recursion
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs 

-- using the standard max function
maximum2 :: (Ord a) => [a] -> a  
maximum2 [] = error "maximum of empty list"  
maximum2 [x] = x  
maximum2 (x:xs) = max x (maximum2 xs)

-- edge condition: elems to replicate must be <= 0 (n)
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x 

-- edge condition 1: if elems to take is <= 0 return an empty list
-- edge condition 2: if list to take elems from is empty (_ []) reurn an empty list
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 

-- edge condition: reverse an empty list
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

-- generates an infinite list. TRY: take' 4 (repeat' 3)
repeat' :: a -> [a]  
repeat' x = x:repeat' x 

-- two edge conditions: one of the two lists is empty
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

-- edge condition: empty list
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs 

-- quick sort algorithm
-- Check out this example [5,1,9,4,6,7,3] -> http://s3.amazonaws.com/lyah/quicksort.png
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
