module Rx.Subscription where

import Rx.Types

import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef


instance Eq Subscription where
    s == t = subscriptions s == subscriptions t

createSubscription :: IO () -> IO Subscription
createSubscription a = do
    b  <- newIORef False
    ss <- newIORef []  
    return $ Subscription a ss b   

emptySubscription :: IO Subscription
emptySubscription = createSubscription (return ())

isUnsubscribed :: Subscription -> IO Bool
isUnsubscribed s = readIORef $ isUnsubscribed_ s

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    b <- isUnsubscribed s
    unless b $ do
        atomicWriteIORef (isUnsubscribed_ s) True
        onUnsubscribe s 
        subs <- readIORef $ subscriptions s
        mapM_ unsubscribe subs 

addSubscription :: Subscription -> Subscription -> IO ()
addSubscription s s' = do 
    b <- isUnsubscribed s
    unless b $ atomicModifyIORef' (subscriptions s) $ \ss -> (s':ss, ())

removeSubscription :: Subscription -> Subscription -> IO ()
removeSubscription s s' = do 
    b <- isUnsubscribed s
    unless b $ atomicModifyIORef' (subscriptions s) $ \ss -> (delete s' ss, ())    