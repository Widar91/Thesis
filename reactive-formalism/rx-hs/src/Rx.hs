module Rx 
    ( Observer (..)
    , Observable (..)    
    , Subscription (..)
    
    -- Observable Creation
    , observableCreate
    , observableNever
    , observableEmpty
    , observableError
    , observableOf
    , observableFrom
    , observableRange
    , observableRangeWithStep
    --, observableInterval
    --, observableTimer

    -- Functions
    , createObserver
    , createSubscription
    , subscribe

    -- Subscription
    , unsubscribe
    , isUnsubscribed
    , addSubscription
    , removeSubscription    

    -- Operators
    , Operators.lift
    , Operators.rxMap
    , Operators.rxFlatMap
    , Operators.rxCombineLatest
    , Operators.rxTake
    , Operators.rxTakeUntil
    , Operators.rxTakeWhile
    ) where

import Rx.Types
import Rx.Observable
import qualified Rx.Operators as Operators
import Rx.Subscription

import Control.Concurrent.STM
import Control.Exception
import Control.Monad


-------------------------------------------------------------------------------
-------------------------------- CORE FUNCTIONS -------------------------------
-------------------------------------------------------------------------------

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = subscribe_ obs safeObserver
    where 
        safeObserver = Observer safeOn safeOe safeOc s
        s            = subscription obr
        safeOn a     = ifSubscribed $ onNext obr a
        safeOe e     = ifSubscribed $ finally (onError obr e) (unsubscribe s)
        safeOc       = ifSubscribed $ onCompleted obr >> unsubscribe s
        ifSubscribed = (>>=) (isUnsubscribed s) . flip unless

createObserver :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO (Observer a)
createObserver on oe oc = createObserver_ on oe oc (return ())

createObserver_ :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO () -> IO (Observer a)
createObserver_ on oe oc a = do 
    s <- createSubscription a
    return $ Observer on oe oc s
