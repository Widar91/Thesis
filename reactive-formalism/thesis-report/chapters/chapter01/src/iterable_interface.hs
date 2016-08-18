-- Java Iterable
class Iterable a where
    getIterator :: () -> IO (Iterator a)

class Iterator a where
    hasNext :: () -> Bool
    next    :: () -> IO a  

---------------------------------------------

-- C# IEnumerable
class IEnumerable a where
    getEnumerator :: () -> IO (IEnumerator a)

class IEnumerator a where
    moveNext :: () -> IO Bool 
    current  :: () -> a  