import Data.Char

-- bind return value to a variable (same as an IO operation)
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b

-- this represents the same as above
-- main = do  
--    let a = "hell"  
--        b = "yeah"  
--    putStrLn $ a ++ " " ++ b
