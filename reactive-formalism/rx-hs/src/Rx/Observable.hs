module Rx.Observable where

import Rx.Types
import Rx.Operators

import Control.Applicative
import Control.Monad


instance Functor Observable where
    fmap = flip rxMap
         
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
    return = pure
    (>>=)  = rxFlatMap
-- shady business, ask Erik    

-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
instance Monoid (Observable a) where
    mempty = Observable onCompleted
    mappend = undefined 

instance Alternative Observable where
    empty = Observable onCompleted
    (<|>) = undefined 

instance MonadPlus Observable where
    mzero = Observable onCompleted
    mplus = (<|>) 


