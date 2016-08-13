import Data.List (all, groupBy)
import Data.Function (on)
import Data.Char

pwd1 = all isAlphaNum "gabi123"
pwd2 = all isAlphaNum "gabi.volpe"

-- simulate words function of Data.List
wordsSim = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

a1 = generalCategory ' '
a2 = generalCategory 'A'
a3 = generalCategory 'a'
a4 = generalCategory '.'
a5 = generalCategory '5'
a6 = map generalCategory " \t\nA9?|"

b1 = map digitToInt "34538"
b2 = map digitToInt "FF85AB" 

c1 = intToDigit 15
c2 = intToDigit 5 

d1 = ord 'a'  
d2 = chr 97  
d3 = map ord "abcdefgh"

-- kinda Caesar cipher
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted

encodeCompose :: Int -> String -> String
encodeCompose shift msg = map (chr . (+ shift) . ord) msg

decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg
