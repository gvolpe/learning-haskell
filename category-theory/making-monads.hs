import Control.Monad
import Data.List (all)
import Data.Ratio

-- Rational data type
a1 = 1%4  
a2 = 1%2 + 1%2  
a3 = 1%3 + 5%4

-- number of probabilities per number
b1 = [(3,1%2),(5,1%4),(9,1%4)]

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

-- trying out the Functor instance
c1 = fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)]) 

flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])

test = getProb flipThree
