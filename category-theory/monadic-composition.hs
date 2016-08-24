import Control.Monad

f = (+1) . (*100)  
a1 = f 4  

g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
a2 = Just 4 >>= g

f2 = foldr (.) id [(+1),(*100),(+1)]  
b1 = f2 1

-- composition for the knight's quest
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start
