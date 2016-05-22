{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE GADTs #-}

module Rx where

import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.STM

import Control.Monad
import Control.Applicative
import Data.Functor.Contravariant
import Data.Monoid

import Data.List
import Data.Maybe
import Data.Typeable



----- TYPES -----

newtype Observable a = Observable { onSubscribe :: Observer a -> IO () }

data Observer a = Observer { onNext       :: a -> IO ()
                           --, onError      :: forall e . Exception e => e -> IO ()
                           , onError      :: SomeException -> IO ()
                           , onCompleted  :: IO ()
                           , subscription :: Subscription
                           }

--data Observer a where     
--    Observer :: Exception e =>  { onNext :: a -> IO ()
--                                , onError' :: e -> IO ()
--                                , onCompleted :: IO ()
--                                , subscription :: Subscription 
--                                } -> Observer a

--onError (Observer _ e _ _) = e 


data Subscription = Subscription { onUnsubscribe :: IO ()
                                 , subscriptions :: TMVar [Subscription] 
                                 }
instance Eq Subscription where
    s == t = subscriptions s == subscriptions t

----- INSTANCE DECLARATION -----

instance Contravariant Observer where
    contramap f obr = Observer (handle (onError obr) . onNext obr . f)
                               (onError obr)
                               (onCompleted obr)
                               (subscription obr)

instance Functor Observable where
    fmap f = lift (contramap f)
        
    
instance Applicative Observable where
    pure a    = Observable (\obr -> onNext obr a >> onCompleted obr)
    fs <*> as = rxCombineLatest (flip ($)) as fs
 

-- Monad Laws
-- return a >>= k  =  k a
-- m >>= return  =  m
-- m >>= (x -> k x >>= h)  =  (m >>= k) >>= h

--Furthermore, the Monad and Applicative operations should relate as follows:
-- pure = return
-- (<*>) = ap

-- The above laws imply:
-- fmap f xs  =  xs >>= return . f
-- (>>) = (*>)
instance Monad Observable where
    return    = pure
    obs >>= f = Observable $ \downstream ->
        let
            outerOnNext gate activeRef hasError hasCompleted v = join . atomically $ do
                active <- takeTMVar activeRef
                putTMVar activeRef (active+1)
                return $ do 
                    ss <- newTMVarIO []
                    let s = Subscription (return ()) ss
                    let inner = Observer (innerOnNext gate hasError)
                                         (outerOnError hasError)
                                         (innerOnCompleted activeRef hasCompleted s)                    
                                         (s)
                    -- here we can check for the downstream sub to not be unsubscribed before doing all this job
                    atomically $ addSubscription (subscription downstream) (subscription inner)
                    handle (outerOnError hasError) $ onSubscribe (f v) inner
                where
                    innerOnNext gate hasError v = do
                        lock <- takeMVar gate
                        handle (outerOnError hasError) $ onNext downstream v
                        putMVar gate lock

                    innerOnCompleted activeRef hasCompleted s = join . atomically $ do
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
            activeRef    <- newTMVarIO 0
            hasError     <- newTMVarIO False
            hasCompleted <- newTMVarIO False

            onSubscribe obs $ Observer (outerOnNext gate activeRef hasError hasCompleted)
                                       (outerOnError hasError)
                                       (outerOnCompleted activeRef hasCompleted)
                                       (subscription downstream) 

-- shady business, ask Erik    

-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
instance Monoid (Observable a) where
    mempty = Observable onCompleted
    mappend = rxMerge 

instance Alternative Observable where
    empty = Observable onCompleted
    (<|>) = rxMerge 

instance MonadPlus Observable where
    mzero = Observable onCompleted
    mplus = (<|>) 




----- INTERNAL FUNCTIONS -----

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

isUnsubscribed :: Subscription -> STM Bool
isUnsubscribed s = fmap isNothing (tryReadTMVar $ subscriptions s)


----- CORE FUNCTIONS -----

subscribe :: Observable a -> Observer a -> IO Subscription
subscribe obs obr = onSubscribe obs (safeObserver obr) >> return (subscription obr)

unsubscribe :: Subscription -> IO ()
unsubscribe s = do
    subs <- atomically (tryTakeTMVar $ subscriptions s)
    when (isJust subs) $ do
        onUnsubscribe s
        mapM_ unsubscribe (fromJust subs)
        

newObserver :: (a -> IO ()) -> (SomeException -> IO ()) -> IO () -> IO (Observer a)
newObserver onNext onError onCompleted = do 
    ss <- newTMVarIO []
    let s = Subscription (return ()) ss
    return $ Observer onNext onError onCompleted s

-- race condition in between the subscription check and the call to the functions
safeObserver :: Observer a -> Observer a
safeObserver obs = Observer safeOnNext safeOnError safeOnCompleted s
    where
        s = subscription obs

        safeOnNext a = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle safeOnError (onNext obs a)

        safeOnError e = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ finally (onError obs e) (unsubscribe s)

        safeOnCompleted = do 
            unsubscribed <- atomically (isUnsubscribed s)
            when (not unsubscribed) $ handle safeOnError (onCompleted obs >> unsubscribe s)
            


----- OPERATORS -----

lift :: (Observer b -> Observer a) -> Observable a -> Observable b
lift f obs = Observable (onSubscribe obs . f)

-- probably not needed
rxMap :: (a -> b) -> Observable a -> Observable b
rxMap = fmap


rxFilter :: (a -> Bool) -> Observable a -> Observable a
rxFilter p = lift (coFilter p)
    where
        coFilter p obr = Observer (\a -> handle (onError obr) $ when (p a) (onNext obr a))
                                  (onError obr)
                                  (onCompleted obr)
                                  (subscription obr)

-- make this take varargs instead of list?
rxOf :: [a] -> Observable a
rxOf as = Observable (\obr -> sequence_ (map (onNext obr) as) >> onCompleted obr)

rxMerge :: Observable a -> Observable a -> Observable a
rxMerge obs obs' = rxOf [obs, obs'] >>= id

-- without a type signature, the internal functions are not polymorphic
rxCombineLatest :: (a -> b -> r) -> Observable a -> Observable b -> Observable r
rxCombineLatest combiner oa ob = Observable $ \downstream -> 
    let
        onNextCL :: TMVar t -> TMVar s -> (t -> s -> IO ()) -> t -> IO ()
        onNextCL refT refS onNextFunc valT = join . atomically $ do
            tryTakeTMVar refT
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


----- TESTS -----

streamI :: Observable Integer
streamI = Observable (\observer -> do onNext observer 1
                                      onNext observer 2
                                      onNext observer 3 
                                      onCompleted observer)

streamS :: Observable Integer
streamS = Observable (\observer -> do onNext observer 4
                                      onNext observer 5
                                      onNext observer 6
                                      onCompleted observer)

streamF :: Observable (Integer -> Integer)
streamF = pure (+1)

printObserver :: Show a => String -> IO (Observer a)
printObserver s = newObserver print (\e -> print $ "printObserver - error: " ++ show e) (print s) 

data MyException = MyException deriving (Show, Typeable)
instance Exception MyException
 
main :: IO ()
main = do 
    --printObserver "doneCL"    >>= subscribe (rxCombineLatest (+) streamI streamS)
    --printObserver "doneAppl"  >>= subscribe (streamF <*> streamI)
    --printObserver "doneAppl2" >>= subscribe (pure (\x -> "Transformed " ++ show x) <*> streamI) 
    --printObserver "doneBind" >>= subscribe (streamI >>= (\x -> Observable (\obr -> onNext obr x >> onNext obr x >> onCompleted obr)))
    
    --still not working
    o <- printObserver "doneBind" 
    subscribe (streamI >>= (\x -> Observable (\obr -> onNext obr x >> onError obr (throw MyException) >> onCompleted obr))) o
    
    --printObserver "done1" >>= subscribe streamI
    --printObserver "done2" >>= subscribe (rxMap (+10) streamI)

    --printObserver "done2" >>= subscribe (rxMap (`div` 0) streamI)
    --printObserver "done2" >>= subscribe (rxMap (\v -> assert (v==2) True) streamI)
    
    --printObserver "doneMerge" >>= subscribe (rxMerge streamI streamS)

    --printObserver "done3" >>= subscribe (rxFilter (>1) streamI)
    --printObserver "done4" >>= subscribe (rxFilter (error "error") streamI)
    --printObserver "done5" >>= subscribe (rxFilter (\v -> v `div` 0 == 1) streamI)
    return ()

    