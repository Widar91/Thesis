type Iterable a = () -> IO (() -> IO a)
type Iterator a = () -> IO a

type Observable a = (a -> IO ()) -> IO () 
type Observer a = a -> IO () 
