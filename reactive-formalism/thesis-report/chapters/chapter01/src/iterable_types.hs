type Iterable a = () -> IO (Iterator a)
type Iterator a = () -> IO (Either Exception (Maybe a))

type Iterable a = () -> IO (() -> IO a)
type Iterator a = () -> IO a