class Iterable a where
    getIterator :: () -> IO (Iterator a)
class Iterator a where
    moveNext :: () -> IO (Either Exception (Maybe a))