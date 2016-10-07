newtype Observable a = Observable 
    { _subscribe :: Observer a -> IO Subscription
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
    fmap f ooa = Observable $ _subscribe ooa . contramap f

lift :: Observable a -> (Observer b -> Observer a) -> Observable b
lift ooa f = Observable $ \ob -> _subscribe ooa (f ob)

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
        ifSubscribed a = isUnsubscribed s >>= flip unless a

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
    { _schedule      :: IO () -> IO Subscription
    , _scheduleDelay :: IO () -> TimeInterval -> IO Subscription
    , subscription  :: Subscription
    }

newThread :: IO Scheduler
newThread = do
    ch  <- newTChanIO
    tid <- forkIO $ forever $ do
        join $ atomically $ readTChan ch
        yield
    sub <- createSubscription (killThread tid)

    return $ Scheduler (schedule ch) (scheduleD ch) sub
        where
            schedule  ch io = 
                atomically $ writeTChan ch io
                emptySubscription
            scheduleD ch io d = do
                threadDelay $ toMicroSeconds d
                schedule_ ch io

observeOn :: Observable a -> IO Scheduler -> Observable a
observeOn o schedIO = Observable $ \obr -> do
    sched <- schedIO
    sub   <- subscription obr 
    liftIO $ addSubscription sub (subscription sched)
    _subscribe o (f s obr)
        where
            f s downstream = Observer 
                {   onNext       = void . _schedule sched . onNext downstream
                ,   onError      = void . _schedule sched . onError downstream
                ,   onCompleted  = void . _schedule sched $ onCompleted downstream
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

