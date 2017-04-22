import System.IO
import Control.Monad
import Data.Time
import Data.IORef
import Data.List
import Data.Maybe
import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Tiempo
import Data.Functor.Contravariant

newtype Observable a = Observable 
    { _subscribe :: Observer a -> IO Subscription
    }
data Observer a = Observer 
    { onNext       :: a -> IO ()
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    , subscription :: Subscription
    }

lift :: (Observer b -> Observer a) -> Observable a -> Observable b
lift f ooa = Observable $ \ob -> _subscribe ooa (f ob)

instance Contravariant Observer where 
    contramap f ob = 
        Observer (onNext ob . f) (onError ob) (onCompleted ob) (subscription ob)   

instance Functor Observable where
    fmap f = lift (contramap f)

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

addSubscription :: Subscription -> Subscription -> IO ()
addSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> s':ss

removeSubscription :: Subscription -> Subscription -> IO ()
removeSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> delete s' ss

-------------------------------------------------------------------------------

observable :: (Observer a -> IO ()) -> Observable a
observable f = Observable $ \obr -> f obr >> return (subscription obr)

observer :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO () -> IO (Observer a)
observer on oe oc a = do 
    s <- createSubscription a
    return $ Observer on oe oc s

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = _subscribe obs safeObserver
    where 
        safeObserver = Observer safeOn safeOe safeOc s
        s            = subscription obr
        safeOn a     = ifSubscribed $ onNext obr a
        safeOe e     = ifSubscribed $ finally (onError obr e) (unsubscribe s)
        safeOc       = ifSubscribed $ onCompleted obr >> unsubscribe s
        ifSubscribed = (>>=) (isUnsubscribed s) . flip unless

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    writeIORef (_isUnsubscribed s) True
    onUnsubscribe s
    subs <- readIORef $ subscriptions s
    mapM_ unsubscribe subs

-- subscribe :: Observable a 
--           -> (a -> IO ())
--           -> (SomeException -> IO ()) 
--           -> IO () 
--           -> IO Subscription
-- subscribe obs onNext onError onCompleted = do 
--     s <- emptySubscription
--     _subscribe obs $ Observer (on' s) (oe' s) (oc' s) s
--     where 
--         on' s a = ifSubscribed s (onNext a)
--         oe' s e = ifSubscribed s (finally (onError e) (unsubscribe s))
--         oc' s   = ifSubscribed s (onCompleted >> unsubscribe s)
--         ifSubscribed s a = isUnsubscribed s >>= flip unless a



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
    _subscribe o (f s obr)
        where
            f s downstream = Observer 
                {   onNext       = void . _schedule s . onNext downstream
                ,   onError      = void . _schedule s . onError downstream
                ,   onCompleted  = void . _schedule s $ onCompleted downstream
                ,   subscription = subscription downstream
                }

-------------------------------------------------------------------------------

instance Applicative Observable where
    pure x = Observable $ \obr -> do
        onNext obr x 
        onCompleted obr 
        return $ subscription obr
    (<*>) = combineLatest ($) 

combineLatest :: (a -> b -> r) -> Observable a -> Observable b -> Observable r
combineLatest combiner oa ob = Observable $ \downstream -> 
    let
        onNext_ :: TMVar t -> TMVar s -> (t -> s -> IO ()) -> t -> IO ()
        onNext_ refT refS onNextFunc valT = join . atomically $ do
            _ <- tryTakeTMVar refT
            putTMVar refT valT 
            maybeS <- tryReadTMVar refS
            return . when (isJust maybeS) $ onNextFunc valT (fromJust maybeS)

        onError_ :: TMVar Bool -> SomeException -> IO ()
        onError_ hasError e = join . atomically $ do
            hasE <- takeTMVar hasError
            putTMVar hasError True
            return . when (not hasE) $ onError downstream e

        onCompleted_ :: TMVar t -> TMVar Bool -> TMVar Int-> IO ()
        onCompleted_ refT hasCompleted hasActive = join . atomically $ do
            emptyT <- isEmptyTMVar refT
            hasC   <- takeTMVar hasCompleted
            active <- takeTMVar hasActive
            putTMVar hasCompleted True
            putTMVar hasActive (active - 1)
            return . when (emptyT && not hasC || active - 1 == 0) $ 
                onCompleted downstream
    in do
        active       <- newTMVarIO 2
        refA         <- newEmptyTMVarIO
        refB         <- newEmptyTMVarIO
        hasError     <- newTMVarIO False
        hasCompleted <- newTMVarIO False
        let obrA = Observer (onNext_ refA refB (fa downstream)) 
                            (onError_ hasError) 
                            (onCompleted_ refA hasCompleted active) 
                            (subscription downstream)
        let obrB = Observer (onNext_ refB refA (fb downstream)) 
                            (onError_ hasError) 
                            (onCompleted_ refB hasCompleted active) 
                            (subscription downstream)
        _subscribe ob obrB
        _subscribe oa obrA
        where
            fa downstream = (\a b -> onNext downstream (combiner a b))
            fb downstream = (\b a -> onNext downstream (combiner a b))

instance Monad Observable where
    return = pure
    (>>=)  = flatMap

flatMap :: Observable a -> (a -> Observable b) -> Observable b
flatMap obs f = Observable $ \downstream ->
    let
        onNext_ gate activeRef hasError hasCompleted val = do
            atomically $ modifyTVar activeRef (+1)
            s <- emptySubscription
            let inner = Observer (innerOnNext_)
                                 (onError_ hasError)
                                 (innerOnCompleted_ s)                    
                                 (s)
            addSubscription (subscription downstream) (subscription inner)
            handle (onError_ hasError) . void $ _subscribe (f val) inner
                where
                    innerOnNext_ v = do
                        withMVar gate $ \_ -> onNext downstream v    

                    innerOnCompleted_ s = do
                        cond <- atomically $ do 
                            c <- readTVar hasCompleted
                            modifyTVar activeRef (subtract 1)
                            a <- readTVar activeRef
                            return (c && a == 0)
                        if cond 
                            then onCompleted downstream
                            else removeSubscription (subscription downstream) s
            
        onError_ hasError e = do
            cond <- atomically $ do 
                e <- swapTVar hasError True
                return (not e)
            when cond $ onError downstream e
        
        onCompleted_ activeRef hasCompleted = do
            cond <- atomically $ do
                c <- swapTVar hasCompleted True
                a <- readTVar activeRef
                return (not c && a == 0)
            when cond $ onCompleted downstream

    in do 
        gate         <- newMVar ()
        activeRef    <- newTVarIO (0 :: Int) 
        hasError     <- newTVarIO False
        hasCompleted <- newTVarIO False

        _subscribe obs $ Observer (onNext_ gate activeRef hasError hasCompleted)
                                  (onError_ hasError)
                                  (onCompleted_ activeRef hasCompleted)
                                  (subscription downstream)

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

main2 :: IO ()
main2 = do
    hSetBuffering stdout LineBuffering
    obr <- observer on oe oc (print "unsubscribed")
    sub <- subscribe obs' obr
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

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    obr <- observer on oe oc (print "unsubscribed")
    sub <- subscribe obs' obr
    return ()
    where 
        obs'  = combineLatest (,) (infinite `observeOn` newThread) (infinite `observeOn` newThread)
        on v = do
            tid <- myThreadId 
            print (show tid ++ ": " ++ show v)
        oe = print . show
        oc = print "Completed"
