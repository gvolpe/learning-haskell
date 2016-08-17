-- few examples on List, Mabe and Either
a1 = fmap (replicate 3) [1,2,3,4]  
a2 = fmap (replicate 3) (Just 4)  
a3 = fmap (replicate 3) (Right "blah")  
a4 = fmap (replicate 3) Nothing  
a5 = fmap (replicate 3) (Left "foo")

----------- functor laws in action 

-- identity function (\x -> x). Formerly: fmap id = id
b1 = fmap id (Just 3)
b2 = id (Just 3)  
b3 = fmap id [1..5]  
b4 = id [1..5]  
b5 = fmap id []  
b6 = fmap id Nothing

-- composition: fmap (f . g) F = fmap f (fmap g F)
