type Iterable a = () -> IO (Iterator a)
type Iterator a = () -> IO (Either SomeException (Maybe a))

type Iterable a = () -> IO (() -> IO a)
type Iterator a = () -> IO a