import Control.Monad

-- left identity: return x >>= f == f x
a1 = return 3 >>= (\x -> Just (x+100000))
a2 = (\x -> Just (x+100000)) 3
leftIdentity = a1 == a2 

-- right identity: m >>= return == m
b1 = Just "move on up" >>= (\x -> return x)  
b2 = [1,2,3,4] >>= (\x -> return x)  
b3 = putStrLn "Wah!" >>= (\x -> return x)

-- associativity: (m >>= f) >>= g == m >>= (\x -> f x >>= g)
c1 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
c2 = ((return (0,0) >>= landRight 2) >>= landLeft 2) >>= landRight 2
--c3 = return (0,0) >>= (\x -> 
--	landRight 2 x >>= (\y -> 
--	landLeft 2 y >>= (\z -> 
--	landRight 2 z)))
-- needs the functions landRight and landLeft defined in monad.hs
associativity = c1 == c2 -- == c3

-- composing functions using Monads
f x = [x,-x]  
g x = [x*3,x*2]  
h = f <=< g  
example = h 3  
