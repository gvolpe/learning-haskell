import Control.Monad.Error

-- definition of the error monad on top of Either
-- instance (Error e) => Monad (Either e) where  
--    return x = Right x   
--    Right x >>= f = f x  
--    Left err >>= f = Left err  
--    fail msg = Left (strMsg msg)  -- the strMsg function is defined in the Error class

a1 = Left "boom" >>= \x -> return (x+1)  
a2 = Right 100 >>= \x -> Left "no way!"

-- very important to define the type at the end!!!
b1 = Right 3 >>= \x -> return (x + 100) :: Either String Int
