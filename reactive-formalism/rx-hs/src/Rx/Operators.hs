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
        outerOnNext gate activeRef hasError hasCompleted val = join . atomically $ do
            active <- takeTMVar activeRef
            putTMVar activeRef (active+1)
            return $ do 
                s <- createSubscription (return ())
                let inner = Observer (innerOnNext)
                                     (outerOnError hasError)
                                     (innerOnCompleted s)                    
                                     (s)
                -- here we can check for the downstream sub to not be unsubscribed before doing all this job
                _ <- atomically $ addSubscription (subscription downstream) (subscription inner)
                handle (outerOnError hasError) $ onSubscribe (f val) inner
            where
                innerOnNext v = do
                    lock <- takeMVar gate
                    handle (outerOnError hasError) $ onNext downstream v
                    putMVar gate lock

                innerOnCompleted s = join . atomically $ do
                    completed <- readTMVar hasCompleted
                    active    <- takeTMVar activeRef
                    putTMVar activeRef (active-1)
                    return $ if completed && active - 1 == 0 
                                then onCompleted downstream 
                                else atomically $ void (removeSubscription (subscription downstream) s)
        
        outerOnError hasError e = join . atomically $ do
            hasE <- takeTMVar hasError
            putTMVar hasError True
            return . when (not hasE) $ onError downstream e
        
        outerOnCompleted activeRef hasCompleted = join . atomically $ do
            completed <- takeTMVar hasCompleted
            active    <- readTMVar activeRef
            putTMVar hasCompleted True
            return . when (not completed && active == 0) $ onCompleted downstream
    in do 
        gate         <- newMVar ()
        activeRef    <- newTMVarIO (0 :: Integer) 
        hasError     <- newTMVarIO False
        hasCompleted <- newTMVarIO False

        onSubscribe obs $ Observer (outerOnNext gate activeRef hasError hasCompleted)
                                   (outerOnError hasError)
                                   (outerOnCompleted activeRef hasCompleted)
                                   (subscription downstream) 


-- without a type signature, the internal functions are not polymorphic
rxCombineLatest :: (a -> b -> r) -> Observable a -> Observable b -> Observable r
rxCombineLatest combiner oa ob = Observable $ \downstream -> 
    let
        onNextCL :: TMVar t -> TMVar s -> (t -> s -> IO ()) -> t -> IO ()
        onNextCL refT refS onNextFunc valT = join . atomically $ do
            _ <- tryTakeTMVar refT
            putTMVar refT valT 
            maybeS <- tryReadTMVar refS
            return . when (isJust maybeS) $ handle (onError downstream) (onNextFunc valT (fromJust maybeS))

        onErrorCL :: TMVar Bool -> SomeException -> IO ()
        onErrorCL hasError e = join . atomically $ do
            hasE <- takeTMVar hasError
            putTMVar hasError True
            return . when (not hasE) $ onError downstream e

        onCompletedCL :: TMVar t -> TMVar Bool -> TMVar Integer -> IO ()
        onCompletedCL refT hasCompleted hasActive = join . atomically $ do
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
        let obrA = Observer (onNextCL refA refB (\a b -> onNext downstream (combiner a b))) 
                            (onErrorCL hasError) 
                            (onCompletedCL refA hasCompleted active) 
                            (subscription downstream)
        let obrB = Observer (onNextCL refB refA (\b a -> onNext downstream (combiner a b))) 
                            (onErrorCL hasError) 
                            (onCompletedCL refB hasCompleted active) 
                            (subscription downstream)
        onSubscribe ob obrB
        onSubscribe oa obrA



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
-- take
-- takeUntil
-- takeWhile
-- untilTake
-- throttle
-- toIterable
-- toList
-- withlatestFrom
-- zip
-- zipWithBuffer
-- window