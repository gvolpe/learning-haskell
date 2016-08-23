import Control.Monad.Instances

-- Monad instance for functions (also called Reader Monad) 
--instance Monad ((->) r) where  
--    return x = \_ -> x  
--    h >>= f = \w -> f (h w) w

-- example using the function monad (in fact it's the Reader Monad itself)
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)

a1 = addStuff 2

-- redefined using let in
addStuffB :: Int -> Int  
addStuffB x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b
