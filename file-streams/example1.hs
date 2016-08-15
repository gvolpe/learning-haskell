import Data.Char  
  
-- same as io/example8 but using getContents
main = do  
    contents <- getContents  
    putStr (map toUpper contents)
