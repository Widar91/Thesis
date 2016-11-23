newtype Iterable a = Iterable
    { getIterator :: () -> IO (Iterator a)
    }
newtype Iterator a = Iterator
    { moveNext :: () -> IO (Either SomeException (Maybe a))
    }