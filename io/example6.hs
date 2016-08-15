import Data.Char

main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()

-- same control using the Control Monad 'when'

--import Control.Monad   
  
--main = do  
--    c <- getChar  
--    when (c /= ' ') $ do  
--        putChar c  
--        main
