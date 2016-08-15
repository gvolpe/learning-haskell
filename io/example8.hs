import Control.Monad  
import Data.Char  
  
-- execute an IO function forever
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
