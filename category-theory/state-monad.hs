import System.Random
import Control.Monad.State

type Stack = [Int]  
  
popS :: Stack -> (Int,Stack)  
popS (x:xs) = (x,xs)  
  
pushS :: Int -> Stack -> ((),Stack)  
pushS a xs = ((),a:xs)

stackManipS :: Stack -> (Int, Stack)  
stackManipS stack = let  
    ((),newStack1) = pushS 3 stack  
    (a ,newStack2) = popS newStack1  
    in popS newStack2

a1 = stackManipS [5,8,2,1]

-- definition of the State Monad
-- newtype State s a = State { runState :: s -> (a,s) }

-- pop and push redefined using the state monad
pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop

b1 = runState stackManip [5,8,2,1]

-- another example
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8

c1 = runState stackStuff [9,0,2,1,0]

-- stick the two functions together
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()

d1 = runState moreStack [1,6,2,4]

-- definitions of get and put in the state monad
-- get = State $ \s -> (s,s)
-- put newState = State $ \s -> ((),newState)

stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]

e1 = runState stackyStack [8,2,5,3]

-- randomness with the state monad
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)

f1 = runState threeCoins (mkStdGen 33)
