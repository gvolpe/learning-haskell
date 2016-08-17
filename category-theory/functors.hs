-- Defined in the standard library
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)

-- defined in Control.Monad.Instances
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))

-- same as aboved using composition
instance Functor ((->) r) where  
    fmap = (.)
