import qualified Data.Set as Set
import Data.List (nub)

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

set1 = Set.fromList text1  
set2 = Set.fromList text2

inter = Set.intersection set1 set2
diff = Set.difference set1 set2
union = Set.union set1 set2

-- very similar to map
a1 = Set.null Set.empty  
a2 = Set.null $ Set.fromList [3,4,5,5,4,3]  
a3 = Set.size $ Set.fromList [3,4,5,3,4,5]  
a4 = Set.singleton 9  
a5 = Set.insert 4 $ Set.fromList [9,3,8,1]  
a6 =  Set.insert 8 $ Set.fromList [5..10]  
a7 =  Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  

-- subset
b1 = Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
b2 = Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
b3 = Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
b4 = Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]

-- map and filter
c1 = Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
c2 = Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  

-- duplicates
setNub xs = Set.toList $ Set.fromList xs  
d1 = setNub "HEY WHATS CRACKALACKIN"  
d2 = nub "HEY WHATS CRACKALACKIN"
