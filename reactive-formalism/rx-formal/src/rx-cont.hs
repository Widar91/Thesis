{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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


-- data Observable a = Observable { onSubscribe :: (Event a -> IO ()) -> IO ()  }

type Observable a = StateT Subscription (ContT () IO) (Event a)

type Observer a = Event a -> IO ()


-- sum type :: Maybe (Either a e)
data Event a = 
      OnNext a 
    | OnError SomeException 
    | OnCompleted

data Subscription = Subscription 
    { onUnsubscribe :: IO ()
    , subscriptions :: TMVar [Subscription] 
    }

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = do 
    s <- createSubscriptionIO (print "unsubscribed")
    runContT (runStateT obs s) $ \(ev,s) -> obr ev
    return s

observable :: (Observer a -> IO ()) -> Observable a
observable = lift . ContT

observer :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> Observer a
observer on oe oc = \ev -> case ev of 
    OnNext v    -> on v
    OnError e   -> oe e
    OnCompleted -> oc

obs :: Observable Int
obs = observable $ \obr -> do 
    obr $ OnNext 1
    obr $ OnNext 2
    obr OnCompleted


obr :: Observer Int
obr ev = case ev of 
    OnNext v    -> print v
    OnError e   -> print . show $ e
    OnCompleted -> print "done"

-- newtype Cont r a = Cont { runCont :: (a -> r) -> r }
-- newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
-- instance Monad (Cont r) where
--     return a = Cont ($ a)
--     m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c

-- This is the function that you pass to lift in the other impl
--   a -> Cont r b
-- ~ a -> (b -> r) -> r    -- Expand the definition of Cont
-- ~ (b -> r) -> (a -> r)  -- Flip the arguments
-- ~ Observer b -> Observer a
-- ~ contramap?
-- lift :: Observable a -> (Observer b -> Observer a) -> Observable b
-- lift obs f = Observable (onSubscribe obs . f)
-- lift obs f = Observable (\observer -> onSubscribe obs (f observer))
-- now in the current implementation:
-- lift :: Observable a -> (Observer b -> Observer a) -> Observable b
-- lift :: ContT () IO (Event a) -> ( (Event b -> IO ()) -> (Event a -> IO ()) ) -> ContT () IO (Event b)
-- lift :: ContT () IO (Event a) -> ( (Event b -> IO ()) -> Event a -> IO () )   -> ContT () IO (Event b)
-- lift :: ContT () IO (Event a) -> ( Event a -> (Event b -> IO ()) -> IO () )   -> ContT () IO (Event b)
-- lift :: ContT () IO (Event a) -> ( Event a -> ContT () IO b )                 -> ContT () IO (Event b)
-- THEREFORE lift = (>>=) in the Cont Monad
-- lift obs f = ContT $ \ob :: (Event b -> IO ()) -> runContT obs (\a -> runContT (f a) ob)

-- (>>=) :: 
--     (a -> ()) -> ()
--     a -> (b -> ()) -> ()
--     (b -> ()) -> ()
-- (>>=) ma f = \ob -> ma ((flip f) ob)
-- lift obs f = ContT $ \ob -> runContT obs (\a -> runContT (f a) ob)

-- rxflatmap :: Observable a -> (a -> Observable b) -> Observable b
-- different from lift
-- rxflatmap :: ContT () IO (Event a) -> (      a -> ContT () IO b) -> ContT () IO (Event b)
-- lift      :: ContT () IO (Event a) -> (Event a -> ContT () IO b) -> ContT () IO (Event b)
-- so lift is not the common known lift from Haskell, it really just is bind in the continuation monad
-- liftO :: Observable o => (a -> r) -> o a -> o r
-- which really is fmap since Observable is a functor.

-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
-- instance Monad m => Monad (StateT s m) where
--     return :: Event a -> StateT Subscription (ContT () IO) (Event a)
--     return :: Event a -> Subscription -> ContT () IO (Event a, Subscription)
--     return x = StateT (\s -> return (x,s))
--     (>>=) :: StateT Subscription (ContT () IO) (Event a) -> (a -> StateT Subscription (ContT () IO) (Event b)) -> StateT Subscription (ContT () IO) (Event b)
--     (>>=) :: (Subscription -> ContT () IO (Event a, Subscription)) -> (a -> Subscription -> ContT () IO (Event b, Subscription)) -> Subscription -> ContT () IO (Event b, Subscription)
--     (StateT c) >>= g = StateT $ \s -> do
--         (v,s') <- c s
--         runStateT (g v) s'
--     (StateT cont) >>= g = StateT $ \s -> m s >>= \(v,s') -> runStateT $ g v s'

--     lift :: ContT () IO (Event a) -> ContT () IO (Event a, Subscription)
--     lift m s = m >>= \a -> return (a,s)
        


rxmap :: Observable a -> (a -> b) -> Observable b
rxmap o f = o >>= mapCont
    where
        mapCont ev = observable $ \downstream ->
            let
                onNext v    = handle onError $ downstream (OnNext (f v))
                onError e   = downstream (OnError e)
                onCompleted = downstream OnCompleted
            in case ev of
                OnNext v    -> onNext v
                OnError e   -> onError e
                OnCompleted -> onCompleted

rxflatmapSynch :: Observable a -> (a -> Observable b) -> Observable b
rxflatmapSynch o f = o >>= fmcont 
    where 
        fmcont ev = observable $ \downstream -> 
            let
                onNext v =  do
                    s' <- createSubscriptionIO (print "unsubscribed inner")
                    handle onError $ runContT (runStateT (f v) s') $ \(ev,s) -> inner ev
                    where 
                        inner = observer onNext_ onError (return ()) 
                        onNext_ v = downstream (OnNext v)
                onError e = downstream (OnError e)
                onCompleted = downstream OnCompleted
            in case ev of
                OnNext v    -> onNext v
                OnError e   -> onError e
                OnCompleted -> onCompleted

-- still not safe for threadpool (e.g. oncompleted -> synch method calls)
rxflatmap :: Observable a -> (a -> Observable b) -> Observable b
rxflatmap o f = do
    s <- get
    gate   <- liftIO $ newMVar ()
    active <- liftIO $ newTVarIO (0 :: Int) 
    err    <- liftIO $ newTVarIO False
    compl  <- liftIO $ newTVarIO False
    
    let 
        flatmapCont ev = observable $ \downstream -> 
            let 
                onNext v = do
                    s'  <- atomically $ do
                        modifyTVar active (+1)
                        s' <- createSubscription (print "unsubscribed inner")
                        addSubscription s s'
                        return s'
                    handle (onError) $ runContT (runStateT (f v) s') inner
                    where 
                        inner (ev_, s_) = case ev_ of
                            OnNext v    -> onNext_ v
                            OnError e   -> onError e
                            OnCompleted -> onCompleted_
                            where
                                onNext_ v = withMVar gate $ \_ -> 
                                    handle onError $ downstream (OnNext v)    
                                onCompleted_ = do
                                    cond <- atomically $ do
                                        c <- readTVar compl
                                        modifyTVar active (subtract 1)
                                        a <- readTVar active
                                        return (c && a == 0)
                                    if cond 
                                        then downstream OnCompleted
                                        else atomically $ void (removeSubscription s s_)            
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
            in case ev of
                OnNext v    -> onNext v
                OnError e   -> onError e
                OnCompleted -> onCompleted

    o >>= flatmapCont


main :: IO ()
main = do
    subscribe (obs `rxflatmap` (\v -> obs)) obr
    return ()




------------------------------------------------------------------------------------

instance Eq Subscription where
    s == t = subscriptions s == subscriptions t

createSubscriptionIO :: IO () -> IO Subscription
createSubscriptionIO = atomically . createSubscription

createSubscription :: IO () -> STM Subscription
createSubscription a = do
    ss <- newTMVar []  
    return $ Subscription a ss   

isUnsubscribed :: Subscription -> STM Bool
isUnsubscribed s = fmap isNothing (tryReadTMVar $ subscriptions s)

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    subs <- atomically (tryTakeTMVar $ subscriptions s)
    when (isJust subs) $ do
        onUnsubscribe s
        mapM_ unsubscribe (fromJust subs)

addSubscription :: Subscription -> Subscription -> STM Bool
addSubscription s s' = do 
    let refSS = subscriptions s
    ss <- tryTakeTMVar refSS
    if isJust ss 
        then putTMVar refSS (s': fromJust ss) >> return True 
        else return False

removeSubscription :: Subscription -> Subscription -> STM Bool
removeSubscription s s' = do 
    let refSS = subscriptions s
    ss <- tryTakeTMVar refSS
    if isJust ss 
        then putTMVar refSS (delete s' $ fromJust ss) >> return True
        else return False





