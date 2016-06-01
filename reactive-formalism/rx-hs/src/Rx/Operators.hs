{-# LANGUAGE ScopedTypeVariables #-}

module Rx.Operators where

import Rx.Types
import Rx.Observer
import Rx.Subscription

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Functor.Contravariant
import Data.Maybe

lift :: Observable a -> (Observer b -> Observer a) -> Observable b
lift obs f = Observable (onSubscribe obs . f)

-- probably not needed
rxMap :: Observable a -> (a -> b) -> Observable b
rxMap oa = lift oa . contramap 

-- why is mfilter only defined on MonadPlus?
rxFilter :: (a -> Bool) -> Observable a -> Observable a
rxFilter = undefined -- mfilter

-- rxMerge :: Observable a -> Observable a -> Observable a
-- rxMerge obs obs' = join $ rxOf [obs, obs']

rxFlatMap :: Observable a -> (a -> Observable b) -> Observable b
rxFlatMap obs f = Observable $ \downstream ->
    let
        onNext_ gate activeRef hasError hasCompleted val = join . atomically $ do
            active <- takeTMVar activeRef
            putTMVar activeRef (active+1)
            return $ do 
                s <- createSubscription (return ())
                let inner = Observer (innerOnNext_)
                                     (onError_ hasError)
                                     (innerOnCompleted_ s)                    
                                     (s)
                -- here we can check for the downstream sub to not be unsubscribed before doing all this job
                -- probably better to use the subscribe so we make the observer safe. this way if the downstream
                -- is unsubscribed, the inner sub will be as well and inneronnext wont go though. this is useless
                -- if the first comment is implememnted, which is better.
                _ <- atomically $ addSubscription (subscription downstream) (subscription inner)
                handle (onError_ hasError) $ onSubscribe (f val) inner
            where
                innerOnNext_ v = do
                    lock <- takeMVar gate
                    handle (onError_ hasError) $ onNext downstream v
                    putMVar gate lock

                innerOnCompleted_ s = join . atomically $ do
                    completed <- readTMVar hasCompleted
                    active    <- takeTMVar activeRef
                    putTMVar activeRef (active-1)
                    return $ if completed && active - 1 == 0 
                                then onCompleted downstream 
                                else atomically $ void (removeSubscription (subscription downstream) s)
        
        onError_ hasError e = join . atomically $ do
            hasE <- takeTMVar hasError
            putTMVar hasError True
            return . when (not hasE) $ onError downstream e
        
        onCompleted_ activeRef hasCompleted = join . atomically $ do
            completed <- takeTMVar hasCompleted
            active    <- readTMVar activeRef
            putTMVar hasCompleted True
            return . when (not completed && active == 0) $ onCompleted downstream
    in do 
        gate         <- newMVar ()
        activeRef    <- newTMVarIO (0 :: Int) 
        hasError     <- newTMVarIO False
        hasCompleted <- newTMVarIO False

        onSubscribe obs $ Observer (onNext_ gate activeRef hasError hasCompleted)
                                   (onError_ hasError)
                                   (onCompleted_ activeRef hasCompleted)
                                   (subscription downstream) 


-- without a type signature, the internal functions are not polymorphic
rxCombineLatest :: (a -> b -> r) -> Observable a -> Observable b -> Observable r
rxCombineLatest combiner oa ob = Observable $ \downstream -> 
    let
        onNext_ :: TMVar t -> TMVar s -> (t -> s -> IO ()) -> t -> IO ()
        onNext_ refT refS onNextFunc valT = join . atomically $ do
            _ <- tryTakeTMVar refT
            putTMVar refT valT 
            maybeS <- tryReadTMVar refS
            return . when (isJust maybeS) $ handle (onError downstream) (onNextFunc valT (fromJust maybeS))

        onError_ :: TMVar Bool -> SomeException -> IO ()
        onError_ hasError e = join . atomically $ do
            hasE <- takeTMVar hasError
            putTMVar hasError True
            return . when (not hasE) $ onError downstream e

        onCompleted_ :: TMVar t -> TMVar Bool -> TMVar Int-> IO ()
        onCompleted_ refT hasCompleted hasActive = join . atomically $ do
            emptyT <- isEmptyTMVar refT
            hasC   <- takeTMVar hasCompleted
            active <- takeTMVar hasActive
            putTMVar hasCompleted True
            putTMVar hasActive (active - 1)
            return . when (emptyT && not hasC || active - 1 == 0) $ onCompleted downstream
    in do
        active       <- newTMVarIO 2
        refA         <- newEmptyTMVarIO
        refB         <- newEmptyTMVarIO
        hasError     <- newTMVarIO False
        hasCompleted <- newTMVarIO False
        let obrA = Observer (onNext_ refA refB (\a b -> onNext downstream (combiner a b))) 
                            (onError_ hasError) 
                            (onCompleted_ refA hasCompleted active) 
                            (subscription downstream)
        let obrB = Observer (onNext_ refB refA (\b a -> onNext downstream (combiner a b))) 
                            (onError_ hasError) 
                            (onCompleted_ refB hasCompleted active) 
                            (subscription downstream)
        onSubscribe ob obrB
        onSubscribe oa obrA

rxTake :: Observable a -> Int -> Observable a
rxTake obs n = Observable $ \downstream -> 
    let  
        onNext_ remainingRef v = join . atomically $ do
            remaining <- fmap pred $ readTVar remainingRef
            writeTVar remainingRef remaining
            return $ action remaining
            where
                action r
                    | r > 0     = onNext downstream v
                    | r == 0    = onNext downstream v >> onCompleted downstream
                    | otherwise = return () 
    in 
        if n <= 0 then onCompleted downstream
        else do 
            remainingRef <- newTVarIO n
            let obr = Observer (onNext_ remainingRef)
                               (onError downstream)
                               (onCompleted downstream)
                               (subscription downstream)
            onSubscribe obs obr

rxTakeUntil :: Observable a -> (a -> Bool) -> Observable a
rxTakeUntil obs p = rxTakeUntil_ obs p True

rxTakeWhile :: Observable a -> (a -> Bool) -> Observable a
rxTakeWhile obs p = rxTakeUntil_ obs (not . p) False

rxTakeUntil_ :: Observable a -> (a -> Bool) -> Bool -> Observable a
rxTakeUntil_ obs p includeLast = Observable $ \downstream -> 
    let  
        onNext_ completedRef v = join . atomically $ do
            completed <- readTVar completedRef
            let shouldComplete = p v
            if not completed && shouldComplete 
                then writeTVar completedRef True
                else return ()
            return $ action completed shouldComplete
            where
                action c sc
                    | not c && not sc = onNext downstream v
                    | not c && sc     = if includeLast
                                            then onNext downstream v >> onCompleted downstream
                                            else onCompleted downstream
                    | otherwise       = return () 
    in do 
        completedRef <- newTVarIO False
        let obr = Observer (onNext_ completedRef)
                           (onError downstream)
                           (onCompleted downstream)
                           (subscription downstream)
        onSubscribe obs obr

-- buffer :: Int -> Observable [a]
-- bufferWithSkip :: Int -> Int -> Observable [a]
-- catchOnError :: Observable a -> (SomeException -> Observable a) -> Observable a
-- concat :: Observable a -> Observable a -> Observable a
-- concatMap = flatmap
-- distinctUntilChanged
-- observeOn
-- ofType
-- onErrorResumeNext
-- doOnNext
-- doOnError
-- doOnCompleted
-- publish
-- retry
-- sample
-- scanleft
-- skip 
-- share
-- skipUntil
-- skipWhile
-- startsWith
-- takeUntil :: Observable a -> Observable b -> Observable a
-- untilTake :: 
-- throttle
-- toIterable
-- toList
-- withlatestFrom
-- zip
-- zipWithBuffer
-- window