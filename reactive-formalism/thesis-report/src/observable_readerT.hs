import Control.Monad.Cont

type Observer   a = Event a -> IO ()
type Observable a = ReaderT Subscription (ContT () IO) (Event a)

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = runContT (runReaderT obs s) safeObserver
    where 
        safeObserver = newObserver safeOn safeOe safeOc
        s            = subscription obr
        safeOn a     = ifSubscribed $ obr (OnNext a)
        safeOe e     = ifSubscribed $ finally (obr $ OnError e) (unsubscribe s)
        safeOc       = ifSubscribed $ obr OnCompleted >> unsubscribe s
        ifSubscribed = (>>=) (isUnsubscribed s) . flip unless

newObservable :: (Observer a -> IO ()) -> Observable a
newObservable = lift . ContT

newObserver :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> Observer a
newObserver on oe oc ev = case ev of 
    OnNext v    -> on v
    OnError e   -> oe e
    OnCompleted -> oc

obs = newObservable $ \observer -> 
    do observer 1
       observer 2
       observer 3

main :: IO ()
main = subscribe obs print

{-
output> 
    1
    2
    3
-}
