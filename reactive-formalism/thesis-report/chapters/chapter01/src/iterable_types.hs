type Iterable a = () -> IO (() -> IO a)
type Iterator a = () -> IO a