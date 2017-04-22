{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import Control.Arrow
import Control.Monad
import Control.Monad.Cont
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Control.Concurrent (ThreadId, forkIO, forkOS, killThread, yield, myThreadId)
import Control.Concurrent.STM.TChan


type Observable a = ReaderT Subscription (ContT () IO) (Event a)
type Observer a   = Event a -> IO ()

data Event a = 
      OnNext a 
    | OnError SomeException 
    | OnCompleted

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = do 
    s <- emptySubscription
    let 
        safeObr = observer safeOnNext safeOnError safeOnCompleted
        safeOnNext a = do 
            b <- isUnsubscribed s
            unless b $ obr (OnNext a)
        safeOnError e = do 
            b <- isUnsubscribed s
            unless b $ finally (obr $ OnError e) (unsubscribe s)
        safeOnCompleted = do 
            b <- isUnsubscribed s
            unless b $ obr OnCompleted >> unsubscribe s
    runContT (runReaderT obs s) safeObr
    return s
 
observable :: (Observer a -> IO ()) -> Observable a
observable os = do
    s <- ask
    lift . ContT $ \downstream -> 
        let     
            -- subscription check not necessary but useful
            on a = do    
                b <- isUnsubscribed s
                unless b $ handle oe (downstream $ OnNext a)
            oe e = do 
                b <- isUnsubscribed s
                unless b $ downstream (OnError e)
            oc = do 
                b <- isUnsubscribed s
                unless b $ handle oe (downstream OnCompleted)
        in     
            os $ observer on oe oc

observer :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> Observer a
observer on oe oc ev = case ev of 
    OnNext v    -> on v
    OnError e   -> oe e
    OnCompleted -> oc

rxmap :: Observable a -> (a -> b) -> Observable b
rxmap o f = o >>= mapCont
    where
        mapCont ev = observable $ \downstream -> case ev of
            OnNext v    -> downstream (OnNext (f v))
            OnError e   -> downstream (OnError e) 
            OnCompleted -> downstream OnCompleted

rxtake :: Observable a -> Int -> Observable a
rxtake o n = do
    nRef <- liftIO $ newIORef n
    o >>= takeFunc nRef
    where 
        takeFunc nRef ev = observable $ \downstream -> case ev of 
            OnNext v    -> do
                n' <- atomicModifyIORef nRef $ pred &&& pred
                when (n' >= 0) $ downstream (OnNext v)
                when (n' == 0) $ downstream OnCompleted
            OnError e   -> downstream (OnError e) 
            OnCompleted -> downstream OnCompleted

rxflatmap :: Observable a -> (a -> Observable b) -> Observable b
rxflatmap o f = do
    s <- ask
    gate   <- liftIO $ newMVar ()
    active <- liftIO $ newTVarIO (0 :: Int) 
    err    <- liftIO $ newTVarIO False
    compl  <- liftIO $ newTVarIO False
    let 
        flatmapCont ev = observable $ \downstream -> 
            let 
                onNext v = do
                    atomically $ modifyTVar active (+1)
                    s_ <- emptySubscription
                    addSubscription s s_
                    let 
                        inner        = observer onNext_ onError onCompleted_
                        onNext_ v_   = withMVar gate $ \_ -> downstream (OnNext v_)    
                        onCompleted_ = do
                            cond <- atomically $ do
                                c <- readTVar compl
                                modifyTVar active (subtract 1)
                                a <- readTVar active
                                return (c && a == 0)
                            if cond 
                                then downstream OnCompleted
                                else removeSubscription s s_   
                    handle onError $ runContT (runReaderT (f v) s_) inner
                onError e = do
                    cond <- atomically $ do 
                        e <- swapTVar err True
                        return (not e)
                    when cond $ downstream (OnError e)
                onCompleted = do
                    cond <- atomically $ do
                        c <- swapTVar compl True
                        a <- readTVar active
                        return (not c && a == 0)
                    when cond $ downstream OnCompleted
            in case ev of
                OnNext v    -> onNext v
                OnError e   -> onError e
                OnCompleted -> onCompleted
    o >>= flatmapCont


------------------------------------------------------------------------------------
-- Subscriptions
------------------------------------------------------------------------------------

data Subscription = Subscription 
    { onUnsubscribe  :: IO ()
    , isUnsubscribed_ :: IORef Bool
    , subscriptions  :: IORef [Subscription] 
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
isUnsubscribed s = readIORef $ isUnsubscribed_ s

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    b <- isUnsubscribed s
    unless b $ do
        writeIORef (isUnsubscribed_ s) True
        onUnsubscribe s
        subs <- readIORef $ subscriptions s
        mapM_ unsubscribe subs        

addSubscription :: Subscription -> Subscription -> IO ()
addSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> s':ss

removeSubscription :: Subscription -> Subscription -> IO ()
removeSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> delete s' ss
    

------------------------------------------------------------------------------------
-- Schedulers
------------------------------------------------------------------------------------

type Scheduler = IO Worker
data Worker = Worker 
    { _schedule      :: IO () -> IO Subscription
    , _subscription  :: Subscription
    }

newThread :: Scheduler
newThread = do
    reqChan <- newTChanIO
    tid <- forkIO $ forever $ do
        join $ atomically $ readTChan reqChan
        yield
    subscription <- createSubscription $ killThread tid

    return Worker 
        { _schedule = \action -> do
            atomically (writeTChan reqChan action)
            emptySubscription
        , _subscription = subscription
        }

observeOn :: Observable a -> Scheduler -> Observable a
observeOn o sched = do 
    s <- ask
    w <- liftIO sched
    liftIO $ addSubscription s (_subscription w)

    let observeOnCont ev = observable $ \downstream -> case ev of
            OnNext v    -> void . _schedule w $ downstream (OnNext v)
            OnError e   -> void . _schedule w $ downstream (OnError e) 
            OnCompleted -> void . _schedule w $ downstream OnCompleted

    o >>= observeOnCont


------------------------------------------------------------------------------------
-- Test
------------------------------------------------------------------------------------

obs :: Observable Int
obs = observable $ \obr -> do 
    obr $ OnNext 1
    obr $ OnNext 2
    obr $ OnNext 3
    obr OnCompleted

obr :: Observer String
obr ev = case ev of 
    OnNext v    -> myThreadId >>= \id -> print (show id ++ ": " ++ v)
    OnError e   -> print . show $ e
    OnCompleted -> print "done"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering  
    -- s <- subscribe (obs `rxflatmap` (\v -> obs `rxmap` (\v' -> "v: " ++ show v ++ " v': " ++ show v'))) obr 
    s <- subscribe (obs `rxtake` 2 `rxmap` (+1) `observeOn` newThread `rxflatmap` (\v -> obs `rxmap` (\v' -> "v: " ++ show v ++ " v': " ++ show v'))) obr
    myThreadId >>= \tid -> putStrLn ("Main Thread: " ++ show tid)
    --yield
    --unsubscribe s
    return () 


