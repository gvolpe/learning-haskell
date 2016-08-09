-- functions type definition
factorial :: Integer -> Integer  
factorial n = product [1..n] 

circumference :: Float -> Float  
circumference r = 2 * pi * r

circumference' :: Double -> Double  
circumference' r = 2 * pi * r

-- read
aa = read "[1,2,3,4]" ++ [3]
bb = read "8.2" + 3.8
cc = read "5" :: Double -- Type annotation

-- enumeration
dd = ['a'..'e']  
ee = [LT .. GT]  
ff = [3..5]  
before = pred 'G'
after = succ 'B'  

-- bounds
b1 = minBound :: Int  
b2 = maxBound :: Char  
b3 = maxBound :: Bool  
b4 = minBound :: Bool

-- integral number type
integralLength = fromIntegral (length [1,2,3,4]) + 3.2 -- just because of lenght function definition is length :: [a] -> Int then it cna't be mixed with a Float
