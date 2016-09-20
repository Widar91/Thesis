import System.IO
import Control.Monad
import Data.Time
import Data.IORef
import Data.List
import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Tiempo
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

instance Contravariant Observer where 
    contramap f ob = Observer (onNext ob . f) (onError ob) (onCompleted ob) (subscription ob)   

instance Functor Observable where
    fmap f ooa = Observable $ onSubscribe ooa . contramap f

lift :: Observable a -> (Observer b -> Observer a) -> Observable b
lift ooa f = Observable $ \ob -> onSubscribe ooa (f ob)

-------------------------------------------------------------------------------

data Subscription = Subscription 
    { onUnsubscribe   :: IO ()
    , _isUnsubscribed :: IORef Bool
    , subscriptions   :: IORef [Subscription] 
    }


instance Eq Subscription where
    s == t = subscriptions s == subscriptions t

createSubscription :: IO () -> IO Subscription
createSubscription a = do
    b  <- newIORef False  
    ss <- newIORef []  
    return $ Subscription a b ss   

emptySubscription :: IO Subscription
emptySubscription = createSubscription (return ())

isUnsubscribed :: Subscription -> IO Bool
isUnsubscribed s = readIORef $ _isUnsubscribed s

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    writeIORef (_isUnsubscribed s) True
    onUnsubscribe s
    subs <- readIORef $ subscriptions s
    mapM_ unsubscribe subs

addSubscription :: Subscription -> Subscription -> IO ()
addSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> s':ss

removeSubscription :: Subscription -> Subscription -> IO ()
removeSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> delete s' ss

-------------------------------------------------------------------------------

subscribe :: Observable a 
          -> (a -> IO ())
          -> (SomeException -> IO ()) 
          -> IO () 
          -> IO Subscription
subscribe obs onNext onError onCompleted = do 
    s <- createSubscription (print "Ubsubscribed")
    onSubscribe obs $ Observer (on' s) (oe' s) (oc' s) s
    where 
        on' s a = isUnsubscribed s >>= flip unless (onNext a)
        oe' s e = isUnsubscribed s >>= flip unless (finally (onError e) (unsubscribe s))
        oc' s   = isUnsubscribed s >>= flip unless (onCompleted >> unsubscribe s)
 
observable :: (Observer a -> IO ()) -> Observable a
observable f = Observable $ \obr -> f obr >> return (subscription obr)

observer :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO (Observer a)
observer on oe oc = do 
    s <- emptySubscription
    return $ Observer on oe oc s

-------------------------------------------------------------------------------

data Scheduler = Scheduler 
    { _schedule      :: IO () -> IO ()
    , _scheduleDelay :: TimeInterval -> IO () -> IO ()
    }

newThread :: IO Scheduler
newThread = do
    ch  <- newTChanIO
    tid <- forkIO $ forever $ do
        join $ atomically $ readTChan ch
        yield
    return $ Scheduler (schedule_ ch) (scheduleDelay_ ch)
        where
            schedule_ ch io = atomically $ writeTChan ch io
            scheduleDelay_ ch delay io = do
                threadDelay $ toMicroSeconds delay
                atomically $ writeTChan ch io

observeOn :: Observable a -> IO Scheduler -> Observable a
observeOn o sched = Observable $ \obr -> do
    s <- sched
    onSubscribe o (f s obr)
        where
            f s downstream = Observer 
                {   onNext       = void . _schedule s . onNext downstream
                ,   onError      = void . _schedule s . onError downstream
                ,   onCompleted  = void . _schedule s $ onCompleted downstream
                ,   subscription = subscription downstream
                }

-------------------------------------------------------------------------------

obs = observable $ \obr -> 
    do onNext obr 1
       onNext obr 2
       onNext obr 3
       onCompleted obr

infinite = observable $ \obr -> do
    counter <- newIORef (1 :: Int)
    repeatN 10 $ do  
        n <- readIORef counter
        onNext obr n
        modifyIORef' counter (+1)
    where 
        repeatN :: Int -> IO () -> IO ()
        repeatN 0 a = return ()
        repeatN n a = do
            a 
            repeatN (n-1) a

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    sub <- subscribe obs' on oe oc
    tid <- myThreadId 
    putStrLn $ "MainThreadId: " ++ show tid
    threadDelay 2000000
    unsubscribe sub
    where 
        obs'  = infinite `rxmap` (+0) `observeOn` newThread `rxmap` (+0)
        rxmap = flip fmap
        on v = do
            threadDelay 1000000
            tid <- myThreadId 
            print (show tid ++ ": " ++ show v)
        oe = print . show
        oc = print "Completed"

