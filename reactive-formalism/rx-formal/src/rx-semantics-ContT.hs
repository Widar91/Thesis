{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Cont
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.State.Lazy
import Control.Exception
import Data.IORef
import Data.List
import Data.Maybe


-- Observable a = (a -> IO ()) -> IO () 
-- Observer   a =  a -> IO () 

-- type Observable a = ContT () IO (a, Observable a)
type Observable a = ContT () IO a

type Observer a = a -> IO ()



subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

-- observable :: ((a -> IO ()) -> IO ()) -> Observable a
observable :: (Observer a -> IO ()) -> Observable a
observable = ContT

rxmap :: (a -> b) -> Observable a -> Observable b
rxmap = fmap

return :: a -> Observable a
return a = ContT ($ a)

-- rxflatmap o f = observable $ \obr -> subscribe o (\a -> subscribe (f a) obr)
rxflatmap :: (a -> Observable b) -> Observable a -> Observable b
rxflatmap = flip (>>=)

rxliftM :: (a1 -> r) -> Observable a1 -> Observable r
rxliftM = fmap
  
rxtake :: Int -> Observable a -> Observable a
rxtake n o = do
    nRef <- liftIO $ newIORef n
    o >>= takeFunc nRef
    where
        takeFunc nRef = \a -> observable $ \downstream -> do
            remaining <- atomicModifyIORef nRef $ \v -> (pred v, v)
            when (remaining > 0) $ downstream a

-- m >>= k  = Cont $ \c -> runCont m $ \a -> runCont (k a) c
------------------------------------------------------------------------------------

main :: IO ()
main = do
    subscribe (rxtake 1 $ fmap (+1) obs) obr
    -- subscribe (obs >>= (\v -> obs)) obr
    -- subscribe (rxCombineLatest const obs obs') obr
    return ()

obs :: Observable Int
obs = observable $ \obr -> do 
    obr 1
    obr 2

obs' :: Observable Int
obs' = observable $ \obr -> do 
    obr 3
    obr 4

obr :: Show a => Observer a
obr = print

-- newtype Cont r a = Cont { runCont :: (a -> r) -> r }
-- newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
-- instance Monad (Cont r) where
--     return a = Cont ($ a)
--     (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
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
    
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------

data Event a = 
      OnNext a 
    | OnError SomeException 
    | OnCompleted

data Subscription = Subscription 
    { onUnsubscribe :: IO ()
    , subscriptions :: TMVar [Subscription] 
    }

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





