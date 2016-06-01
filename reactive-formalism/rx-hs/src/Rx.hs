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


----- CORE FUNCTIONS -----

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = onSubscribe obs safeObr >> return s
    where
        s = subscription obr
        safeObr = Observer safeOnNext safeOnError safeOnCompleted s
        safeOnNext a = do 
            -- race condition in between the subscription check and the call to onNext
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle safeOnError (onNext obr a)
        safeOnError e = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ finally (onError obr e) (unsubscribe s)
        safeOnCompleted = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle safeOnError (onCompleted obr >> unsubscribe s)



createObserver :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO (Observer a)
createObserver on oe oc = do 
    s <- createSubscription (return ())
    return $ Observer on oe oc s


-------------------------------------------------------------------------------
------------------------ OBSERVABLE CREATION FUNCTIONS ------------------------
-------------------------------------------------------------------------------      

-- Same as Observable.create, but adds checks on the subscription so the user
-- does not have to bother. 
observableCreate :: (Observer a -> IO ()) -> Observable a
observableCreate os = Observable $ \downstream ->
    let 
        s = subscription downstream
        semiSafeOnNext a = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle semiSafeOnError (onNext downstream a)
        semiSafeOnError e = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ onError downstream e
        semiSafeOnCompleted = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle semiSafeOnError (onCompleted downstream)
    in     
        do os $ Observer semiSafeOnNext semiSafeOnError semiSafeOnCompleted s

observableNever :: Observable a
observableNever = observableCreate (\obr -> return ())

observableEmpty :: Observable a
observableEmpty = observableCreate onCompleted

observableError :: SomeException -> Observable a
observableError e = observableCreate (\obr -> onError obr e)

-- of and from can be merged in a polyvariadic function
observableOf :: a -> Observable a
observableOf a = observableCreate (\obr -> onNext obr a >> onCompleted obr)

observableFrom :: [a] -> Observable a
observableFrom as = observableCreate (\obr -> mapM_ (onNext obr) as >> onCompleted obr)

observableRange :: Int -> Int -> Observable Int
observableRange b e = observableRangeWithStep b e 1  

observableRangeWithStep :: Int -> Int -> Int -> Observable Int
observableRangeWithStep b e s = observableCreate (\obr -> (mapM_ (onNext obr) . take (e-b) . iterate (+s) $ b) >> onCompleted obr)

-- observableInterval :: Observable Integer
-- observableInterval = undefined

-- observableTimer :: Observable Integer
-- observableTimer = undefined

-- concatAll :: Observable (Observable a) -> Observable a
-- concatAll = undefined

-- defer :: (() -> Observable a) -> Observable a
-- defer = undefined

-- switchOnNext :: Observable (Observable a) -> Observable a
-- switchOnNext = undefined

