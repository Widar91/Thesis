module Rx.Subscription where

import Rx.Types

import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Maybe


instance Eq Subscription where
    s == t = subscriptions s == subscriptions t


-------------------------------------------------------------------------------

createSubscription :: IO () -> IO Subscription
createSubscription a = do
    ss <- newTMVarIO []  
    return $ Subscription a ss   

isUnsubscribed :: Subscription -> STM Bool
isUnsubscribed s = fmap isNothing (tryReadTMVar $ subscriptions s)

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    subs <- atomically (tryTakeTMVar $ subscriptions s)
    when (isJust subs) $ do
        onUnsubscribe s
        mapM_ unsubscribe (fromJust subs)

-- addSubscription only if not unsubscribed can be enforced 
-- using takeTMVar refSS. same goes for removeSubscription.
-- can be used if every onnext is wrapped in a subscription
-- check.
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