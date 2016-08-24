import Control.Applicative
import Control.Monad
import Control.Monad.Writer

-- liftM is for Monads what fmap is for Functors
a1 = liftM (*3) (Just 8)  
a2 = fmap (*3) (Just 8)  
a3 = runWriter $ liftM not $ writer (True, "chickpeas")  
a4 = runWriter $ fmap not $ writer (True, "chickpeas")  
--a5 = runState (liftM (+100) pop) [1,2,3,4]  
--a6 = runState (fmap (+100) pop) [1,2,3,4]

-- definition of liftM
-- liftM :: (Monad m) => (a -> b) -> m a -> m b  
-- liftM f m = m >>= (\x -> return (f x))

-- ap is the same as <*> with Monad class constraint instead of Applicative
-- ap :: (Monad m) => m (a -> b) -> m a -> m b  
-- ap mf m = do  
--    f <- mf  
--    x <- m  
--    return (f x)

-- examples of ap
b1 = Just (+3) <*> Just 4  
b2 = Just (+3) `ap` Just 4  
b3 = [(+1),(+2),(+3)] <*> [10,11]  
b4 = [(+1),(+2),(+3)] `ap` [10,11]

-- the join funcion (flatten in Scala)
-- join :: (Monad m) => m (m a) -> m a

-- m >>= f is always the same thing as join (fmap f m)

c1 = join (Just (Just 9))  
c2 = join (Just Nothing)  
c3 = join Nothing

-- for lists join is the same as concat
c4 = join [[1,2,3],[4,5,6]]

-- for writer is using the mappend from the Monoid class constraint
c5 = runWriter $ join (writer (writer (1,"aaa"),"bbb"))

-- flattening Either
c6 = join (Right (Right 9)) :: Either String Int  
c7 = join (Right (Left "error")) :: Either String Int  
c8 = join (Left "error") :: Either String Int

-- for state monad
-- c9 = runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
-- result: ((),[10,1,2,0,0,0])

-- filter function
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False

e1 = fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
e2 = mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs

f1 = powerset [1,2,3]

-- foldM function
-- foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a

binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)

g1 = foldM binSmalls 0 [2,8,3,1]  
g2 = foldM binSmalls 0 [2,11,3,1]
