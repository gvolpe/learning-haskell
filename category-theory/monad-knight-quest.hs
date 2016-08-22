import Control.Monad

type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')

-- same function defined using list's filter
moveKnightFilter :: KnightPos -> [KnightPos]  
moveKnightFilter (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

a1 = moveKnight (6,2)  
a2 = moveKnight (8,1)

-- all the positions that can be reached in 3 moves
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second

b1 = in3 (6,2)

-- defined without do notation
--in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start

c1 = (6,2) `canReachIn3` (6,1)
c2 = (6,2) `canReachIn3` (7,3)
