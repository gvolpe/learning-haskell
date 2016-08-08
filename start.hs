-- Functions

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100 then x else x * 2  

myNameAsConstant = "Gabi Volpe"

-- Appending and prepending elems

myList = [1,2,3] ++ [4,5,6]

helloWorld = "Hello" ++ " " ++ "World"

helloWorld2 = "Hello " ++ ['W','o'] ++ "rld"

cat = 'A' : " small cat"

indice = "Gabriel Volpe" !! 8

-- Some lists functions

a = head [1,2,3,5]

b = tail [1,2,3,5]

c = last [1,2,3,5]

d = sum [1..99]

-- Infinite collections (Streams)

e = take 10 (cycle [1,2,3])

f = take 10 (repeat 5)

f2 = replicate 10 5 -- same result as f

-- More functions

g = [x*2 | x <- [1..10]]

-- Filtering functions

h = [x*2 | x <- [1..10], x*2 >= 12]

i = [ x | x <- [50..100], x `mod` 7 == 3]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

j = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] 

k = [ x*y | x <- [2,5,10], y <- [8,10,11]]

l = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]

nouns = ["Fender","Ibanez","Gibson"]
adjectives = ["Awesome", "Incredible"]

combination = [adj ++ " " ++ noun | adj <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]   

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

-- Tuples

tuple = (28, "Gabi")
t1 = fst tuple
t2 = snd tuple

zipped = zip [1..5] "abcde"

-- Triangle exercise

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  
