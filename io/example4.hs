import Data.Char

-- return is just a function IO -> WhateverTheTypeIs and the remaining lines will be executed.
-- eg: return "hey" will be IO -> String whereas return () will be IO -> () (or Unit)
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line
