import qualified Foldable as F

-- for List is the same as foldr from Prelude
a1 = foldr (*) 1 [1,2,3]  
a2 = F.foldr (*) 1 [1,2,3]

-- example on Maybe
b1 = F.foldl (+) 2 (Just 9)  
b2 = F.foldr (||) False (Just True)

-- bynary tree examples
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

-- instance for Tree
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r

-- example on Tree
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )

c1 = F.foldl (+) 0 testTree  
c2 = F.foldl (*) 1 testTree

d1 = getAny $ F.foldMap (\x -> Any $ x == 3) testTree
d2 = getAny $ F.foldMap (\x -> Any $ x > 15) testTree

e1 = F.foldMap (\x -> [x]) testTree
