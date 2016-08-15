putStr "Hey"  -- putStr is implemented based on putChar
putChar 'a'

print True -- print is like putStr . show
print 2
print [1,2,3]

-- map an IO function over a list and execute the side effects with sequence
sequence (map print [1,2,3,4,5])

-- this will do he same as above
mapM print [1,2,3,4,5]

-- this one discards the result and executes the function just for the side effects
mapM_ print [1,2,3,4,5]

