-- typeclass declaration using 'data' keyword
data ZipList1 a = ZipList1 [a]

-- typeclass declaration using record syntax
data ZipList2 a = ZipList2 { getZipList2 :: [a] }

-- typeclass declaration using 'newtype' keyword. It's faster than 'data' for types that just wrap an existent type. It can only have one value constructor.
newtype ZipList3 a = ZipList3 { getZipList3 :: [a] }

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

a1 = CharList "this will be shown!"  
a2 = CharList "benny" == CharList "benny"  
a3 = CharList "benny" == CharList "oisters"

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)

b1 = getPair $ fmap (*100) (Pair (2,3))  
b2 = getPair $ fmap reverse (Pair ("london calling", 3))

-- try it by calling helloMe undefined to see the difference between 'data' and 'newtype' keywords
-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"
