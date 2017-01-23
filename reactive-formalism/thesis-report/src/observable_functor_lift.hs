import Data.Functor.Contravariant

newtype Observable a = Observable 
    { onSubscribe :: Observer a -> IO Subscription 
    } 

data Observer a = Observer   
    { onNext       :: a -> IO () 
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    , subscription :: Subscription
    }





lift :: (Observer b -> Observer a) -> Observable a -> Observable b
lift f ooa = Observable $ \ob -> onSubscribe ooa (f ob)

instance Contravariant Observer where 
    contramap f ob = Observer $ onNext ob . f    

instance Functor Observable where
    fmap f = lift (contramap f)

instance Monad Observable where
    (>>=) = undefined
    return = undefined

