main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs

-- the same as above
--main = do  
--    a <- getLine  
--    b <- getLine  
--    c <- getLine  
--    print [a,b,c]

-- map and execute the side effect of the IO sequence
--sequence (map print [1,2,3,4,5])
