import qualified Data.Map as Map
import Data.Char (isUpper)

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]

-- all the findKey functions could be replaced by Data.List (lookup)
unsafeFindKey :: (Eq k) => k -> [(k,v)] -> v  
unsafeFindKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKeyRecursive :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKeyRecursive key [] = Nothing  
findKeyRecursive key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKeyRecursive key xs 

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- functions from Data.Map that provide association lists

phones = Map.fromList phoneBook

-- duplicated keys are discarded
noDup = Map.fromList [(1,2),(3,4),(3,2),(5,5),(1,3)]

-- create maps
a1 = Map.insert 1 5 Map.empty
a2 = Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty

-- own implementation using foldr
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

-- is map empty?
b1 = Map.null Map.empty  
b2 = Map.null $ Map.fromList [(2,3),(5,5)]

--- map size?
c1 = Map.size Map.empty
c2 = Map.size $ Map.fromList [(1,4),(3,2)]

-- one element map
d1 = Map.singleton 3 9  
d2 = Map.insert 5 9 $ Map.singleton 3 9 

-- lookup on map
e1 = Map.lookup 3 d2
e2 = Map.lookup 1 d2 

-- key exists?
f1 = Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]  
f2 = Map.member 3 $ Map.fromList [(2,5),(4,5)]

-- map and filter
g1 = Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]  
g2 = Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]

-- toList from map
h1 = Map.toList d2

-- keys and elems
i1 = Map.keys d2
i2 = Map.elems d2

-- own implentation of keys and elems
keys' :: Map.Map k v -> [k]
keys' m = map fst . Map.toList $ m

elems' :: Map.Map k v -> [v]
elems' m = map snd . Map.toList $ m

--- repeated keys with different values using fromListWith

phoneBook2 =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ] 

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

patsyNumbers = Map.lookup "patsy" $ phoneBookToMap phoneBook2

-- when duplicated values, keep the maximum
keepMaxValue = Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]

-- or sum the duplicated values
sumDupValues = Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]

sumDupValuesOnInsert = Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
