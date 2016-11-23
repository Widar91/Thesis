newtype Iterable a = Iterable
    { getIterator :: () -> IO (Iterator a)
    }

data Iterator a = Iterator
    { moveNext :: () -> IO Bool
    , current  :: () -> a
    }