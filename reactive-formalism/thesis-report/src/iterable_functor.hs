import Control.Monad

{-
    Using a simplified syntax, hiding IO and Haskell's newtypes, 
    in order to clearly show the idea:

    iterator :: () -> a
    iterable :: () -> () -> a

    fmapi ::  (a -> b) -> (() -> a)       -> () -> b
    fmapi         f           ia           = () -> f (ia ())

    fmapii :: (a -> b) -> (() -> () -> a) -> () -> () -> b
    fmapii        f          iia           = () -> fmapi f (iia ())
-}

newtype Iterable a = Iterable { getIterator :: () -> IO (Iterator a) } 
newtype Iterator a = Iterator { runIterator :: () -> IO a } 

instance Functor Iterator where
    fmap f ia = Iterator $ \_ -> liftM f (runIterator ia ()) 

instance Functor Iterable where
    fmap f ia = Iterable $ \_ -> liftM (fmap f) (getIterator ia ()) 

