{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

import System.IO
import Control.Monad
import Control.Monad.Cont
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.State
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe
import Control.Concurrent (ThreadId, forkIO, forkOS, killThread, threadDelay, yield, myThreadId)
import Control.Concurrent.STM.TChan

type Observable a = ContT () (StateT Subscription IO) (Event a)

type Observer a = Event a -> IO ()

data Event a = 
      OnNext a 
    | OnError SomeException 
    | OnCompleted

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = do 
    s <- createSubscription (print "unsubscribed")
    let 
        safeObr = \ev -> do
            sub <- get
            case ev of 
                OnNext v    -> liftIO $ safeOnNext sub v 
                OnError e   -> liftIO $ safeOnError sub e 
                OnCompleted -> liftIO $ safeOnCompleted sub
        safeOnNext s' a = do 
            b <- isUnsubscribed s'
            when (not b) $ obr (OnNext a)
        safeOnError s' e = do 
            b <- isUnsubscribed s'
            when (not b) $ finally (obr $ OnError e) (unsubscribe s')
        safeOnCompleted s' = do 
            b <- isUnsubscribed s'
            when (not b) $ obr OnCompleted >> unsubscribe s'

    (_, sub) <- runStateT (runContT obs safeObr) s
    return sub

observable :: ((Event a -> StateT Subscription IO ()) -> StateT Subscription IO ()) -> Observable a
observable = ContT

observer :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> Observer a
observer on oe oc = \ev -> case ev of 
    OnNext v    -> on v 
    OnError e   -> oe e 
    OnCompleted -> oc 

rxmap :: Show a => Observable a -> (a -> b) -> Observable b
rxmap o f = o >>= mapCont
    where
        mapCont ev = ContT $ \downstream -> do 
            s <- get 
            
            case ev of
                OnNext v    -> (liftIO $ createSubscription (print . show $ v) >>= \s' -> addSubscription s s') >> downstream (OnNext (f v))
                OnError e   -> downstream (OnError e)
                OnCompleted -> downstream OnCompleted
            

-- rxflatmapSynch :: Observable a -> (a -> Observable b) -> Observable b
-- rxflatmapSynch o f = o >>= fmcont 
--     where 
--         fmcont ev = observable $ \downstream -> 
--             let
--                 onNext v =  do
--                     s' <- createSubscriptionIO (print "unsubscribed inner")
--                     handle onError $ runContT (runStateT (f v) s') $ \(ev,s) -> inner ev
--                     where 
--                         inner = observer onNext_ onError (return ()) 
--                         onNext_ v = downstream (OnNext v)
--                 onError e = downstream (OnError e)
--                 onCompleted = downstream OnCompleted
--             in case ev of
--                 OnNext v    -> onNext v
--                 OnError e   -> onError e
--                 OnCompleted -> onCompleted

-- still not safe for threadpool (e.g. oncompleted -> synch method calls)
rxflatmap :: Observable a -> (a -> Observable b) -> Observable b
rxflatmap o f = do
    gate   <- liftIO $ newMVar ()
    active <- liftIO $ newTVarIO (0 :: Int) 
    err    <- liftIO $ newTVarIO False
    compl  <- liftIO $ newTVarIO False
    let 
        flatmapCont ev = ContT $ \downstream -> do
            s <- get
            let 
                --this is where is eventually doesn't work: I need to do IO actions to decide which
                --continuation to call, but the continuation is now a StateT of IO so the types
                --don't work.
                onNext s v = do
                    atomically $ modifyTVar active (+1)
                    s_ <- liftIO $ createSubscription (print "unsubscribed inner")
                    addSubscription s s_
                    let 
                        inner sub ev_ = do
                            s_ <- get
                            case ev_ of
                                OnNext v    -> liftIO $ onNext_ v
                                OnError e   -> liftIO $ onError e
                                OnCompleted -> liftIO $ onCompleted_ sub s_        
                        onNext_ v = withMVar gate $ \_ -> 
                            handle onError $ downstream (OnNext v)    
                        onCompleted_ sub s_ = do
                            cond <- atomically $ do
                                c <- readTVar compl
                                modifyTVar active (subtract 1)
                                a <- readTVar active
                                return (c && a == 0)
                            if cond 
                                then downstream OnCompleted
                                else removeSubscription sub s_
                    handle (onError) $ void $ runStateT (runContT (f v) $ inner s) s_

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
                    when cond $ downstream $ OnCompleted
            case ev of
                OnNext v    -> onNext s v
                OnError e   -> onError e
                OnCompleted -> onCompleted
    o >>= flatmapCont

obs :: Observable Int
obs = observable $ \obr -> do 
    obr $ OnNext 1
    obr $ OnNext 2
    obr $ OnNext 3
    obr OnCompleted

obr :: Observer Int
obr ev = case ev of 
    OnNext v    -> myThreadId >>= print
    OnError e   -> print . show $ e
    OnCompleted -> print "done"

-- main :: IO ()
-- main = do
--     subscribe (obs `rxmap` (+1)) obr
--     return ()

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering  
    -- subscribe (obs `rxflatmap'` (\v -> obs)) obr
    subscribe (obs `observeOn` newThread `rxmap` (+10)) obr
    --subscribe (obs `rxmap` (+1) `rxmap` (+10)) obr
    myThreadId >>= \id -> putStrLn ("Main Thread: " ++ show id)
    return ()




------------------------------------------------------------------------------------

data Subscription = Subscription 
    { onUnsubscribe  :: IO ()
    , isUnsubscribed_ :: IORef Bool
    , subscriptions  :: IORef [Subscription] 
    }

-- instance Monoid Subscription where
--   mempty = Subscription (return ())
--   Subscription a `mappend` Subscription b = Subscription (a *> b)

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
    writeIORef (isUnsubscribed_ s) True
    onUnsubscribe s
    subs <- readIORef $ subscriptions s
    mapM_ unsubscribe subs

addSubscription :: Subscription -> Subscription -> IO ()
addSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> (s':ss)

removeSubscription :: Subscription -> Subscription -> IO ()
removeSubscription s s' = modifyIORef' (subscriptions s) $ \ss -> delete s' ss
    
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

type Scheduler = IO Worker

data Worker = Worker 
    { _schedule      :: StateT Subscription IO () -> StateT Subscription IO Subscription
    --, _scheduleDelay :: TimeInterval -> IO () -> IO Subscription
    , _subscription  :: Subscription
    }

newThread :: Scheduler
newThread = do
    reqChan <- newTChanIO
    tid <- forkIO $ forever $ do
        --here is where it fails, join should be substituted by runStateT, but we don't have an initial state 
        --to run it with. Another option is on line 212 to write not the state, but the IO action, problem is the 
        --same, there is no way to get the action out without disjointing the states.
        join $ atomically $ readTChan reqChan
        yield
    subscription <- createSubscription (killThread tid)

    return $ Worker 
        { _schedule = \state -> do
                -- even if I use mapStateT it won't work, there is no way I can get the state out of the io from 
                -- the other thread in otder to return it
                sub <- get --liftIO $ createSubscription (print "sacrality")
                mapStateT (\io -> atomically (writeTChan reqChan io) >> return (sub, sub)) state
                
        --, _scheduleDelay = \interval innerAction -> do
            --let action = threadDelay (toMicroSeconds interval) >> innerAction
            --atomically (TChan.writeTChan reqChan action)
        --    emptySubscription
        , _subscription = subscription
        }

observeOn :: Observable a -> Scheduler -> Observable a
observeOn o sched = do 
    s <- get
    w <- liftIO $ sched
    liftIO $ addSubscription s (_subscription w)

    let observeOnCont ev = observable $ \downstream -> case ev of
            OnNext v    -> void . _schedule w $ downstream (OnNext v)
            OnError e   -> void . _schedule w $ downstream (OnError e) 
            OnCompleted -> void . _schedule w $ downstream OnCompleted

    o >>= observeOnCont

    


