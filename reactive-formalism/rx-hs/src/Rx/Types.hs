module Rx.Types where

import Control.Concurrent.STM
import Control.Exception


newtype Observable a = Observable 
    { onSubscribe :: Observer a -> IO () 
    }

data Observer a = Observer 
    { onNext       :: a -> IO ()
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    , subscription :: Subscription
    }

data Subscription = Subscription 
    { onUnsubscribe :: IO ()
    , subscriptions :: TMVar [Subscription] 
    }

