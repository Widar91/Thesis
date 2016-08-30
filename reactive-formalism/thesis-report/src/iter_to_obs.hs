{-
    () -> (() -> a) -- iterable
    () <- (() <- a) -- apply duality
    (a -> ()) -> () -- observable
-}

type Iterator a = () -> IO a
             -- = () IO <- a
type Observer a = a  -> IO () 

type Iterable   a = () -> IO (() -> IO a)
               -- = () IO <- (() IO <- a) 
type Observable a = (a -> IO ()) -> IO () 