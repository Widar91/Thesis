type Observable a = ContT () IO a
type Observer a = a -> IO ()

type Observable a = ContT () IO (Either Error (Maybe a))
type Observer a = Either Error (Maybe a) -> IO ()