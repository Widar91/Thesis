{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rx
import Control.Concurrent.STM


main :: IO ()
main = do
    --printObserver "doneCL"    >>= subscribe (rxCombineLatest (+) streamI streamS)
    --printObserver "doneAppl"  >>= subscribe (streamF <*> streamI)
    --printObserver "doneAppl2" >>= subscribe (pure (\x -> "Transformed " ++ show x) <*> streamI) 
    -- printObserver "doneBind" >>= subscribe (streamI >>= (\x -> Observable (\obr -> onNext obr x >> onNext obr x >> onCompleted obr)))
    
    --still not working
    --o <- printObserver "doneBind" 
    --subscribe (streamI >>= (\x -> Observable (\obr -> onNext obr x >> onError obr (throw MyException) >> onCompleted obr))) o
    
    -- printObserver "done1" >>= subscribe (streamI `rxMap` (*100) `rxMap` (+5))
    -- printObserver "done2" >>= subscribe (streamI `rxMap` (+10))

    -- printObserver "done2" >>= subscribe (streamI `rxMap` (`div` 0))
    -- -- printObserver "done2" >>= subscribe (rxMap (\v -> assert (v==2) True) streamI)
    -- printObserver "doneRange" >>= subscribe (observableRange 0 5)

    printObserver "doneTake" >>= subscribe (streamI `rxTake` 1)     
    -- printObserver "doneTakeUntil" >>= subscribe (streamI `rxTakeUntil` (>3))    
    -- printObserver "doneTakeWhile" >>= subscribe (streamI `rxTakeWhile` (<3))    
    -- -- printObserver "doneMerge" >>= subscribe (rxMerge streamI streamS)

    -- printObserver "done3" >>= subscribe (rxFilter (>1) streamI)
    -- printObserver "done4" >>= subscribe (rxFilter (error "error") streamI)
    -- printObserver "done5" >>= subscribe (rxFilter (\v -> v `div` 0 == 1) streamI)
    return ()

streamI :: Observable Int
streamI = observableCreate (\observer -> do onNext observer 1
                                            onNext observer 2
                                            onNext observer 3 
                                            onCompleted observer)

streamS :: Observable Int
streamS = observableCreate (\observer -> do onNext observer 4
                                            onNext observer 5
                                            onNext observer 6
                                            onCompleted observer)

streamF :: Observable (Int -> Int)
streamF = pure (+1)

printObserver :: Show a => String -> IO (Observer a)
printObserver s = createObserver print (\e -> print $ "printObserver - error: " ++ show e) (print s) 

-- subscriptionTest :: IO ()
-- subscriptionTest = do
--     print "Subscription Tests"
--     ss1 <- newTMVarIO []
--     ss2 <- newTMVarIO []
--     let s1 = Subscription (print "unsubscribed s1") ss1
--     let s2 = Subscription (print "unsubscribed s2") ss2
--     obr :: Observer Integer <- printObserver "done1"
--     atomically (addSubscription (subscription obr) s1 >> addSubscription (subscription obr) s2)
--     b <- atomically (isUnsubscribed (subscription obr))
--     print b
--     ss <- atomically $ readTMVar (subscriptions (subscription obr))
--     print $ length ss
--     print "removing subscription s1"
--     --atomically (removeSubscription (subscription obr) s1)
--     unsubscribe s1
--     print "removed s1"
--     ss' <- atomically $ readTMVar (subscriptions (subscription obr))
--     print $ length ss'
--     unsubscribe (subscription obr) 
--     print "unsubscribed"
--     --atomically (addSubscription (subscription obr) s1 >> addSubscription (subscription obr) s2)
--     b <- atomically (isUnsubscribed (subscription obr))
--     print b
--     unsubscribe (subscription obr)
--     b <- atomically (isUnsubscribed (subscription obr))
--     print b
    