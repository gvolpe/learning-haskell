import System.IO

-- reading a file in chunks
main = do   
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)
