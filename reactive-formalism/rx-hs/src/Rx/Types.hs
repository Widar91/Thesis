module Rx.Types where

import Control.Concurrent.STM
import Control.Exception
import Data.IORef


newtype Observable a = Observable 
    { subscribe_ :: Observer a -> IO Subscription 
    }

data Observer a = Observer 
    { onNext       :: a -> IO ()
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    , subscription :: Subscription
    }

data Subscription = Subscription 
    { onUnsubscribe   :: IO ()
    , subscriptions   :: IORef [Subscription] 
    , isUnsubscribed_ :: IORef Bool
    }

