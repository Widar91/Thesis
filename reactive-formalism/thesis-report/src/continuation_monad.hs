newtype Cont r a = Cont { runCont :: ((a -> r) -> r) }
 
instance Monad (Cont r) where 
    return a       = Cont $ \k -> k a                       
    (Cont c) >>= f = Cont $ \k -> c (\a -> runCont (f a) k) 