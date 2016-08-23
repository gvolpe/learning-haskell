import Control.Monad.Writer
import Data.Monoid

-- defining a normal function with a logging context
simpleApplyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
simpleApplyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

a1 = (1, "Starting. ") `simpleApplyLog` (\x -> (x+1, "Applying function +1."))
a2 = ("Tobin","Got outlaw name.") `simpleApplyLog` (\x -> (length x, "Applied length."))

-- applyLog with Monoid class constraint
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- add drink example
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ 	 = ("beer", Sum 30)

-- using apply log
b1 = ("beans", Sum 10) `applyLog` addDrink  
b2 = ("jerky", Sum 25) `applyLog` addDrink  
b3 = ("dogmeat", Sum 5) `applyLog` addDrink
b4 = ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink

-- definition of the Writer Monad in Control.Monad.Writer
--newtype Writer w a = Writer { runWriter :: (a, w) }

-- instance definition
--instance (Monoid w) => Monad (Writer w) where  
--    return x = Writer (x, mempty)  
--    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- using whe Writer pure function (return)
c1 = runWriter (return 3 :: Writer String Int)  
c2 = runWriter (return 3 :: Writer (Sum Int) Int)  
c3 = runWriter (return 3 :: Writer (Product Int) Int)

-- example of do notation 
logNumber :: Int -> Writer [String] Int  
logNumber x = writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"] -- this will add more log to the context without changing the main value
    return (a*b)

d1 = runWriter multWithLog

-- greatest common divisor example
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)

-- examples
e1 = fst $ runWriter (gcd' 8 3) -- getting just the result
e2 = mapM_ putStrLn $ snd $ runWriter (gcd' 8 3) -- showing the logs too

-- gcd in reverse
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result

-- inefficient because uses ++ to append lists to the left instead of to the right
f1 = mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)

-- defining an efficient list for appending values
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

g1 = fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])

-- comparing performance in lists
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])

fast = mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000

finalCountDownSlow :: Int -> Writer [String] ()  
finalCountDownSlow 0 = do  
    tell ["0"]  
finalCountDownSlow x = do  
    finalCountDownSlow (x-1)  
    tell [show x]

slow = mapM_ putStrLn . snd . runWriter $ finalCountDownSlow 500000
