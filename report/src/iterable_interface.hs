-- Java Iterable
newtype Iterable a = Iterable
    { getIterator :: () -> IO (Iterator a)
    }

data Iterator a = Iterator
    { hasNext :: () -> Bool
    , next    :: () -> IO a
    }  

---------------------------------------------

-- C# IEnumerable
newtype IEnumerable a = IEnumerable
    { getEnumerator :: () -> IO (IEnumerator a)
    }

data IEnumerator a = IEnumerator
    { moveNext :: () -> IO Bool
    , current  :: () -> a
    }