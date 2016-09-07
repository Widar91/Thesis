type Observer   a = a -> IO ()
type Observable a = ContT () IO a

type Observer   a = Either SomeException (Maybe a) -> IO ()
type Observable a = ContT () IO (Observer a)