-- typeclass defined in Data.Monoid
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty

-- Monoid laws
-- Identity1: mempty `mappend` x = x
-- Identity2: x `mappend` mempty = x
-- Associativity: (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- instance for List
instance Monoid [a] where  
    mempty = []  
    mappend = (++)

-- examples on List
a1 = [1,2,3] `mappend` [4,5,6]  
a2 = ("one" `mappend` "two") `mappend` "tree"  
a3 = "one" `mappend` ("two" `mappend` "tree")  
a4 = "one" `mappend` "two" `mappend` "tree"  
a5 = "pang" `mappend` mempty  
a6 = mconcat [[1,2],[3,6],[9]]  
a7 = mempty :: [a]

-- Data.Monois defines types for Product and Sum
newtype Product a = Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)

newtype Sum a = Sum { getSum :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)

-- monoid instance for product
instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)

-- monoid instance for sum
instance Num a => Monoid (Sum a) where  
    mempty = Sum 0  
    Sum x `mappend` Sum y = Sum (x + y)

-- examples on product
b1 = getProduct $ Product 3 `mappend` Product 9   
b2 = getProduct $ Product 3 `mappend` mempty  
b3 = getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2  
b4 = getProduct . mconcat . map Product $ [3,4,2]

-- examples on sum
c1 = getSum $ Sum 2 `mappend` Sum 9    
c2 = getSum $ mempty `mappend` Sum 3    
c3 = getSum . mconcat . map Sum $ [1,2,3]

-- terrible name for defining logical OR operation
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)

-- examples on any
d1 = getAny $ Any True `mappend` Any False  
d2 = getAny $ mempty `mappend` Any True  
d3 = getAny . mconcat . map Any $ [False, False, False, True]  
d4 = getAny $ mempty `mappend` mempty

-- yet another nice name to define logical AND operation
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)

-- examples on all
e1 = getAll $ mempty `mappend` All True  
e2 = getAll $ mempty `mappend` All False  
e3 = getAll . mconcat . map All $ [True, True, True]  
e4 = getAll . mconcat . map All $ [True, True, False]

-- instance for Ordering
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT

-- examples on ordering
f1 = LT `mappend` GT  
f2 = GT `mappend` LT  
f3 = mempty `mappend` LT  
f4 = mempty `mappend` GT

-- compare length of two words
lengthCompare :: String -> String -> Ordering 
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a

l1 = lengthCompare "gabi" "volpe"
l2 = lengthCompare "gabriel" "volpe"
l3 = lengthCompare "code" "code"

-- compare length of two words using the Monoid instance
lengthCompareM :: String -> String -> Ordering  
lengthCompareM x y = (length x `compare` length y) `mappend`  
		     (x `compare` y)

lm1 = lengthCompareM "gabi" "volpe"
lm2 = lengthCompareM "gabriel" "volpe"
lm3 = lengthCompareM "code" "code"

-- comparing vowels too
lengthCompareV :: String -> String -> Ordering  
lengthCompareV x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")

lv1 = lengthCompareV "gabi" "volpe"
lv2 = lengthCompareV "gabriel" "volpe"
lv3 = lengthCompareV "code" "code"

-- monoid instance for Maybe
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)

-- examples on Maybe
x1 = Nothing `mappend` Just "andy"  
x2 = Just LT `mappend` Nothing  
x3 = Just (Sum 3) `mappend` Just (Sum 4)  

-- keeping the first value on Maybe. There's a similar type called Last that keepts the second value defined in Data.Monoid
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x

-- examples on first
y1 = getFirst $ First (Just 'a') `mappend` First (Just 'b')  
y2 = getFirst $ First Nothing `mappend` First (Just 'b')  
y3 = getFirst $ First (Just 'a') `mappend` First Nothing
y4 = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]
