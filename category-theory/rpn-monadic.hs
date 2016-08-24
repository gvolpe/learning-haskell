import Control.Monad
import Data.List  

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing

a1 = readMaybe "1" :: Maybe Int  
a2 = readMaybe "GO TO HELL" :: Maybe Int

foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

b1 = foldingFunction [3,2] "*"  
b2 = foldingFunction [3,2] "-"  
b3 = foldingFunction [] "*"  
b4 = foldingFunction [] "1"  
b5 = foldingFunction [] "1 wawawawa"
  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result

c1 = solveRPN "1 2 * 4 +"  
c2 = solveRPN "1 2 * 4 + 5 *"  
c3 = solveRPN "1 2 * 4"  
c4 = solveRPN "1 8 wharglbllargh"
