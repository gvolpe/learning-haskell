-- Applicative definition
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b

-- instance for Maybe
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something

-- instance for List
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]

-- fmap syntax
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x

-- examples
a1 = Just (+3) <*> Just 9  
a2 = pure (+3) <*> Just 10  
a3 = pure (+3) <*> Just 9  
a4 = Just (++"hahah") <*> Nothing  
a5 = Nothing <*> Just "woot"

b1 = pure (+) <*> Just 3 <*> Just 5  
b2 = pure (+) <*> Just 3 <*> Nothing  
b3 = pure (+) <*> Nothing <*> Just 5 

c1 = (++) <$> Just "johntra" <*> Just "volta"
c2 = (++) "johntra" "volta" 

d1 = pure "Hey" :: [String]  
d2 = pure "Hey" :: Maybe String

e1 = [(*0),(+100),(^2)] <*> [1,2,3]
e2 = [(+),(*)] <*> [1,2] <*> [3,4]
e3 = (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]

-- product of 2 lists
f1 = [ x*y | x <- [2,5,10], y <- [8,10,11]] -- list comprehensions
f2 = (*) <$> [2,5,10] <*> [8,10,11] -- applicatives

f3 = filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]

-- instance for IO
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)

-- an IO action
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b

-- same as above using Applicatives
myActionAp :: IO String  
myActionAp = (++) <$> getLine <*> getLine

-- instance for functions
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)

g1 = pure 3 "blah"
g2 = (+) <$> (+3) <*> (*100) $ 5
g3 = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5

