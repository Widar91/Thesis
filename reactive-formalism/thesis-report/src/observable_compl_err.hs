newtype Observer a = Observer 
    { onNext :: Either SomeException (Maybe a) -> IO () 
    } 
