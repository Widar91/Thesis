newtype Observable a = Observable 
    { subscribe :: Observer a -> IO () 
    }

data Observer a = Observer 
    { onNext       :: a -> IO ()
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    }