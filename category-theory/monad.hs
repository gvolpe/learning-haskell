-- defining >>= for Maybe with a fancy name for now
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x

a1 = Just 3 `applyMaybe` \x -> Just (x+1)  
a2 = Just "smile" `applyMaybe` \x -> Just (x ++ " :)")  
a3 = Nothing `applyMaybe` \x -> Just (x+1)  
a4 = Nothing `applyMaybe` \x -> Just (x ++ " :)")
a5 = Just 3 `applyMaybe` \x -> if x > 2 then Just x else Nothing  
a6 = Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing

-- definition of Monad in Haskell (should have an Applicative class constraint but it doesn't...)
--class Monad m where  
--    return :: a -> m a  
--  
--    (>>=) :: m a -> (a -> m b) -> m b  
--  
--    (>>) :: m a -> m b -> m b  
--    x >> y = x >>= \_ -> y  
--  
--    fail :: String -> m a  
--    fail msg = error msg

-- Monad instance for Maybe
--instance MyMonad Maybe where  
--    return x = Just x  
--    Nothing >>= f = Nothing  
--    Just x >>= f  = f x  
--    fail _ = Nothing

-- examples on Maybe
b1 = return "WHAT" :: Maybe String  
b2 = Just 9 >>= \x -> return (x*10)  
b3 = Nothing >>= \x -> return (x*10)

-- interactive example
type Birds = Int  
type Pole = (Birds,Birds)

landLeft1 :: Birds -> Pole -> Pole  
landLeft1 n (left,right) = (left + n,right)  
  
landRight1 :: Birds -> Pole -> Pole  
landRight1 n (left,right) = (left,right + n)

-- try it out!
c1 = landLeft1 2 (0,0)  
c2 = landRight1 1 (1,2)  
c3 = landRight1 (-1) (1,2)
c4 = landLeft1 2 (landRight1 1 (landLeft1 1 (0,0)))

x -: f = f x

-- same as c4 using the function above
c5 = (0,0) -: landLeft1 1 -: landRight1 1 -: landLeft1 2

-- functions defined using Maybe
landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing

-- examples using these functions
d1 = landLeft 2 (0,0)  
d2 = landLeft 10 (0,3)

-- using bind
e1 = landRight 1 (0,0) >>= landLeft 2
e2 = Nothing >>= landLeft 2
e3 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

-- failing in the middle
e4 = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

banana :: Pole -> Maybe Pole  
banana _ = Nothing

e5 = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1

-- using the function >> that ignores the input as banana does
e6 = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1

-- nested bind (>>=)
f1 = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- using do notation (for-comprehension in Scala)
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)

marySue :: Maybe Bool  
marySue = do   
    x <- Just 9  
    Just (x > 8)

routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    second <- landRight 2 first  
    landLeft 1 second

ignored :: Maybe Pole  
ignored = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  -- same to: _ <- Nothing
    second <- landRight 2 first  
    landLeft 1 second

-- using pattern matching in monadic expressions
justH :: Maybe Char  
justH = do  
    (x:xs) <- Just "hello"  
    return x

-- pattern matching that fails, invokes the fail monad function
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x

-- Monad instance for List
-- instance Monad [] where  
--    return x = [x]  
--    xs >>= f = concat (map f xs)  
--    fail _ = []

-- examples on lists
g1 = [3,4,5] >>= \x -> [x,-x]

-- fail is defined as an empty list
g2 = [] >>= \x -> ["bad","mad","rad"]  
g3 = [1,2,3] >>= \_ -> []

-- chaining monadic operations on list
g4 = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

-- same as g4 written using do notation
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)

-- same as g4 using List comprehension (syntax sugar for using lists as monads)
listOfTuplesC = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

-- filtering a list
filteredList = [ x | x <- [1..50], '7' `elem` show x ]

-- MonadPlus definition
class Monad m => MonadPlus m where  
    mzero :: m a  
    mplus :: m a -> m a -> m a

-- MonadPlus instance of List
instance MonadPlus [] where  
    mzero = []  
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero

-- filter example using guard
h1 = guard (5 > 2) :: [()]
h2 = guard (5 > 2) >> return "cool" :: [String]  
h3 = guard (1 > 2) >> return "cool" :: [String]

-- filter list using Monad
filteredListMonadic = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x) -- filter
    return x
