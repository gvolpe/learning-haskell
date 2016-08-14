import Shapes
import qualified Data.Map as Map

sc = surface $ Circle (Point 10 20) 10  
sr = surface $ Rectangle (Point 0 0) (Point 100 100)

-- map over a Circle
mapped = map (Circle (Point 10 20)) [4,5,6,6]

moved = nudge (Circle (Point 34 34) 10) 5 10

movedBaseRect = nudge (baseRect 40 100) 60 23

----- record syntax
--data PersonRS = PersonRS { firstName :: String  
--                     , lastName :: String  
--                     , age :: Int  
--                     , height :: Float  
--                     , phoneNumber :: String  
--                     , flavor :: String  
--                     } deriving (Show)

data Car = Car String String Int deriving (Show)

aCar = Car "Ford" "Mustang" 1967 

-- using record syntax
data CarRS = CarRS {company :: String, model :: String, year :: Int} deriving (Show)

aCarRS = CarRS {company="Ford", model="Mustang", year=1967}

--------- type parameters

-- defining Maybe in the Scala way
data Option a = None | Some a deriving (Show)

positive :: Int -> Option Int
positive x = if x >= 0 then Some x else None

--- defining some functions for Vector
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

v1 = Vector 3 5 8 `vplus` Vector 9 2 8  
v2 = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
v3 = Vector 3 9 7 `vectMult` 10  
v4 = Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
v5 = Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)

---------- typeclasses
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  

eq1 = mca == adRock  
eq2 = mikeD == adRock  
eq3 = mikeD == mikeD  
eq4 = mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43}

beastieBoys = [mca, adRock, mikeD]  
eqInList = mikeD `elem` beastieBoys

-- read
r1 = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
r2 = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
r3 = read "Just 't'" :: Maybe Char

-- Ord (types defined on the right side are bigger than the ones defined on the left side) eg: Data Bool = False | True deriving (Ord)
c1 = True `compare` False  
c2 = True > False  
c3 = True < False

c4 = Nothing < Just 100    
c5 = Nothing > Just (-49999)  
c6 = Just 3 `compare` Just 2  -- here comparing what's inside Just
c7 = Just 100 > Just 50 -- same here

-- Enum (predecessors and successors) and Bounded (lowest and highest possible value)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- show and read
d1 = Wednesday  
d2 = show Wednesday  
d3 = read "Saturday" :: Day

-- eq and ord
d4 = Saturday == Sunday  
d5 = Saturday == Saturday  
d6 = Saturday > Friday  
d7 = Monday `compare` Wednesday

-- bounded
lowestDay = minBound :: Day  
highestDay = maxBound :: Day

-- enum
nextDay = succ Monday  
prevDay = pred Saturday  
dayRange = [Thursday .. Sunday]  
allDays = [minBound .. maxBound] :: [Day]

-------- type synonyms
--type String = [Char]

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]

phoneBook :: PhoneBook  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

-- looks clearer
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]

-- partially applied type constructors
type IntMap = Map.Map Int

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- high-school lockers example
data LockerState = Taken | Free deriving (Show, Eq)  
  
type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

l1 = lockerLookup 102 lockers
l2 = lockerLookup 103 lockers
l3 = lockerLookup 109 lockers

----------- recursive data structures

data MyList1 a = AnEmpty | ACons a (MyList1 a) deriving (Show, Read, Eq, Ord)

m1 = AnEmpty  
m2 = 5 `ACons` AnEmpty  
m3 = 4 `ACons` (5 `ACons` AnEmpty)  
m4 = 3 `ACons` (4 `ACons` (5 `ACons` AnEmpty))

-- number 5 represents the fixity (defines priority in operations)
infixr 5 :-:  
data MyList a = Empty | a :-: (MyList a) deriving (Show, Read, Eq, Ord)

n1 = 3 :-: 4 :-: 5 :-: Empty
n2 = 3 :-: 4 :-: 5 :-: Empty 
n3 = 100 :-: n2

-- defining a function to sum 2 lists
infixr 5  .++  
(.++) :: MyList a -> MyList a -> MyList a   
Empty .++ ys = ys  
(x :-: xs) .++ ys = x :-: (xs .++ ys)

listA = 3 :-: 4 :-: 5 :-: Empty  
listB = 6 :-: 7 :-: Empty  
sumLists = listA .++ listB

-- bynary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

numsTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

inTree1 = 8 `treeElem` numsTree  
inTree2 = 100 `treeElem` numsTree

------- typeclasses 102

-- creating a typeclass
--class Eq a where  
--    (==) :: a -> a -> Bool  
--    (/=) :: a -> a -> Bool  
--    x == y = not (x /= y)  
--    x /= y = not (x == y)

-- creating a type
data TrafficLight = Red | Yellow | Green

-- deriving typeclass Eq manually
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

tl1 = Red == Red
tl2 = Red `elem` [Red, Yellow, Green]

-- derivation of typeclass for type constructor Option
instance Eq m => Eq (Option m) where  
    Some x == Some y = x == y  
    None == None = True  
    _ == _ = False 

-- yes no typeclass (like in js) 
class YesNo a where  
    yesno :: a -> Bool

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True

instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True

instance YesNo Bool where  
    yesno = id -- identity

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True

-- playing with the instances
yn1 = yesno $ length []  
yn2 = yesno "haha"  
yn3 = yesno ""  
yn4 = yesno $ Just 0  
yn5 = yesno True  
yn6 = yesno []  
yn7 = yesno [0,0,0]  

-------- Functor typeclass

-- for deriviation Functor needs a type constructor as a type and not a concrete type

--instance Functor [] where  
--    fmap = map

f1 = fmap (*2) [1..3]

-- map on Maybe
f2 = fmap (*2) (Just 200)  
f3 = fmap (*2) Nothing

-- Functor instance for Tree (our data type above)
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

f4 = fmap (*2) EmptyTree  
f5 = fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])

-- kinds
class Tofu t where  
    tofu :: j a -> t a j

data Frank a b  = Frank {frankField :: b a} deriving (Show)

-- kind:  * -> (* -> *) - > *

x1 = Frank {frankField = Just "HAHA"}
x2 = Frank {frankField = Node 'a' EmptyTree EmptyTree}

instance Tofu Frank where  
    tofu x = Frank x

x3 = tofu (Just 'a') :: Frank Char Maybe  
x4 = tofu ["HELLO"] :: Frank [Char] []

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry a b) where  
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
