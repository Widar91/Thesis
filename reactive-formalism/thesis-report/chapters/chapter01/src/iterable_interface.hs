class Iterable a where
    getIterator :: () -> IO (Iterator a)

-- Java-like Iterator
class Iterator a where
    next    :: () -> IO a  
    hasNext :: () -> Bool

-- C#-like Iterator
class Iterator a where
    current  :: () -> a  
    moveNext :: () -> IO Bool   