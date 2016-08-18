import Control.Applicative

-- instance definition
--instance Applicative ZipList where  
--        pure x = ZipList (repeat x)  
--        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- ZipList doesn't have a Show instance, therefore we use getZipList
a1 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
a2 = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
a3 = getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
a4 = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- liftA2 function
b1 = fmap (\x -> [x]) (Just 4)
b2 = liftA2 (:) (Just 3) (Just [4])  
b3 = (:) <$> Just 3 <*> Just [4]

-- sequence using recursion
sequenceR :: (Applicative f) => [f a] -> f [a]  
sequenceR [] = pure []  
sequenceR (x:xs) = (:) <$> x <*> sequenceR xs

-- sequence using fold
sequenceF :: (Applicative f) => [f a] -> f [a]  
sequenceF = foldr (liftA2 (:)) (pure [])

c1 = sequenceR [Just 1, Just 2]
c2 = sequenceF [Just 1, Just 2]

d1 = sequenceF [Just 3, Just 2, Just 1]  
d2 = sequenceF [Just 3, Nothing, Just 1]  
d3 = sequenceF [(+3),(+2),(+1)] 3  
d4 = sequenceF [[1,2,3],[4,5,6]]  
d5 = sequenceF [[1,2,3],[4,5,6],[3,4,4],[]]

-- verify whether all the elements of a list satisfies a predicate
e1 = map (\f -> f 7) [(>4),(<10),odd]  
e2 = and $ map (\f -> f 7) [(>4),(<10),odd]

-- same as above using applicatives
f1 = sequenceF [(>4),(<10),odd] 7  
f2 = and $ sequenceF [(>4),(<10),odd] 7

-- generating a list of lists 
g1 = sequenceF [[1,2,3],[4,5,6]]  
g2 = [[x,y] | x <- [1,2,3], y <- [4,5,6]]  
g3 = sequenceF [[1,2],[3,4]]  
g4 = [[x,y] | x <- [1,2], y <- [3,4]]  
g5 = sequenceF [[1,2],[3,4],[5,6]]  
g6 = [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]

-- IO action
h1 = sequenceF [getLine, getLine, getLine]
