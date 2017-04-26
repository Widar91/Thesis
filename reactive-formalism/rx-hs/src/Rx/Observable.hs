module Rx.Observable where

import Rx.Types
import Rx.Operators
import Rx.Subscription (isUnsubscribed)

import Control.Applicative
import Control.Monad
import Control.Exception


instance Functor Observable where
    fmap = flip rxMap
         
instance Applicative Observable where
    pure      = observableOf 
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
    return = pure
    (>>=)  = rxFlatMap
-- shady business, ask Erik    

-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
instance Monoid (Observable a) where
    mempty = observableEmpty
    mappend = undefined 

instance Alternative Observable where
    empty = observableEmpty
    (<|>) = undefined 

instance MonadPlus Observable where
    mzero = observableEmpty
    mplus = (<|>) 

-------------------------------------------------------------------------------
------------------------ OBSERVABLE CREATION FUNCTIONS ------------------------
-------------------------------------------------------------------------------      

{-
    Checks on the subscription are not needed but improve performance, avoiding 
    events to be propagated from the producer if the subscription is already 
    unsubscribed.
    Possible errors thrown by operators or functions executed by them do not 
    need to be handled by the operators themselves as they will be bubbled up
    and handled here.

    WARNING: simply handling errors here might cause problems with error
             handling operators.
-}
observableCreate :: (Observer a -> IO ()) -> Observable a
observableCreate os = Observable $ \ds -> 
    let 
        obr  = Observer on oe oc s
        s    = subscription ds
        on a = ifSubscribed $ handle oe (onNext ds a)
        oe e = ifSubscribed $ onError ds e
        oc   = ifSubscribed $ handle oe (onCompleted ds)
        ifSubscribed = (>>=) (isUnsubscribed s) . flip unless
    in do 
        os obr
        return s
    
observableNever :: Observable a
observableNever = observableCreate (\_ -> return ())

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

{-
    Might be wrong on the take, test it!
-}
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

