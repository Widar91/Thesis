{-# LANGUAGE ScopedTypeVariables #-}

module Rx.Operators where

import Rx.Types
import Rx.Observer
import Rx.Subscription

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Arrow
import Data.Functor.Contravariant
import Data.Maybe
import Data.IORef

lift :: Observable a -> (Observer b -> Observer a) -> Observable b
lift obs f = Observable (subscribe_ obs . f)

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
        onNext_ gate activeRef hasError hasCompleted val = do
            atomically $ modifyTVar activeRef (+1)
            s <- emptySubscription
            let inner = Observer (innerOnNext_)
                                 (onError_ hasError)
                                 (innerOnCompleted_ s)                    
                                 (s)
            addSubscription (subscription downstream) (subscription inner)
            handle (onError_ hasError) . void $ subscribe_ (f val) inner
                where
                    innerOnNext_ v = 
                        withMVar gate $ \_ -> onNext downstream v    
                    innerOnCompleted_ s = do
                        cond <- atomically $ do 
                            c <- readTVar hasCompleted
                            modifyTVar activeRef (subtract 1)
                            a <- readTVar activeRef
                            return (c && a == 0)
                        if cond 
                            then onCompleted downstream
                            else removeSubscription (subscription downstream) s     
        onError_ hasError e = do
            cond <- atomically $ do 
                e <- swapTVar hasError True
                return (not e)
            when cond $ onError downstream e 
        onCompleted_ activeRef hasCompleted = do
            cond <- atomically $ do
                c <- swapTVar hasCompleted True
                a <- readTVar activeRef
                return (not c && a == 0)
            when cond $ onCompleted downstream
    in do 
        gate         <- newMVar ()
        activeRef    <- newTVarIO (0 :: Int) 
        hasError     <- newTVarIO False
        hasCompleted <- newTVarIO False

        subscribe_ obs $ Observer (onNext_ gate activeRef hasError hasCompleted)
                                  (onError_ hasError)
                                  (onCompleted_ activeRef hasCompleted)
                                  (subscription downstream)

-- without a type signature, the internal functions are not polymorphic
rxCombineLatest :: (a -> b -> r) -> Observable a -> Observable b -> Observable r
rxCombineLatest = undefined
-- rxCombineLatest combiner oa ob = Observable $ \downstream -> 
--     let
--         onNext_ :: TMVar t -> TMVar s -> (t -> s -> IO ()) -> t -> IO ()
--         onNext_ refT refS onNextFunc valT = join . atomically $ do
--             _ <- tryTakeTMVar refT
--             putTMVar refT valT 
--             maybeS <- tryReadTMVar refS
--             return . when (isJust maybeS) $ onNextFunc valT (fromJust maybeS)

--         onError_ :: TMVar Bool -> SomeException -> IO ()
--         onError_ hasError e = join . atomically $ do
--             hasE <- takeTMVar hasError
--             putTMVar hasError True
--             return . when (not hasE) $ onError downstream e

--         onCompleted_ :: TMVar t -> TMVar Bool -> TMVar Int-> IO ()
--         onCompleted_ refT hasCompleted hasActive = join . atomically $ do
--             emptyT <- isEmptyTMVar refT
--             hasC   <- takeTMVar hasCompleted
--             active <- takeTMVar hasActive
--             putTMVar hasCompleted True
--             putTMVar hasActive (active - 1)
--             return . when (emptyT && not hasC || active - 1 == 0) $ 
--                 onCompleted downstream
--     in do
--         active       <- newTMVarIO 2
--         refA         <- newEmptyTMVarIO
--         refB         <- newEmptyTMVarIO
--         hasError     <- newTMVarIO False
--         hasCompleted <- newTMVarIO False
--         let obrA = Observer (onNext_ refA refB (fa downstream)) 
--                             (onError_ hasError) 
--                             (onCompleted_ refA hasCompleted active) 
--                             (subscription downstream)
--         let obrB = Observer (onNext_ refB refA (fb downstream)) 
--                             (onError_ hasError) 
--                             (onCompleted_ refB hasCompleted active) 
--                             (subscription downstream)
--         _subscribe ob obrB
--         _subscribe oa obrA
--         where
--             fa downstream = (\a b -> onNext downstream (combiner a b))
--             fb downstream = (\b a -> onNext downstream (combiner a b))

rxTake :: Observable a -> Int -> Observable a
rxTake obs n = Observable $ \ds -> 
    let  
        s = subscription ds
        onNext_ nRef v = do
            n' <- atomicModifyIORef' nRef (pred &&& pred)
            when (n' >= 0) $ onNext ds v
            when (n' == 0) $ onCompleted ds
    in 
        if n <= 0 
            then onCompleted ds >> return s
            else do 
                nRef <- newIORef n
                let obr = Observer (onNext_ nRef)
                                   (onError ds)
                                   (onCompleted ds)
                                   (s)
                subscribe_ obs obr

rxTakeUntil :: Observable a -> (a -> Bool) -> Observable a
rxTakeUntil obs p = rxTakeUntil_ obs p True

rxTakeWhile :: Observable a -> (a -> Bool) -> Observable a
rxTakeWhile obs p = rxTakeUntil_ obs (not . p) False

rxTakeUntil_ :: Observable a -> (a -> Bool) -> Bool -> Observable a
rxTakeUntil_ = undefined
-- rxTakeUntil_ obs p includeLast = Observable $ \downstream -> 
--     let  
--         onNext_ completedRef v = join . atomically $ do
--             completed <- readTVar completedRef
--             let shouldComplete = p v
--             if not completed && shouldComplete 
--                 then writeTVar completedRef True
--                 else return ()
--             return $ action completed shouldComplete
--             where
--                 action c sc
--                     | not c && not sc = onNext downstream v
--                     | not c && sc     = if includeLast
--                                             then onNext downstream v >> onCompleted downstream
--                                             else onCompleted downstream
--                     | otherwise       = return () 
--     in do 
--         completedRef <- newTVarIO False
--         let obr = Observer (onNext_ completedRef)
--                            (onError downstream)
--                            (onCompleted downstream)
--                            (subscription downstream)
--         onSubscribe obs obr

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