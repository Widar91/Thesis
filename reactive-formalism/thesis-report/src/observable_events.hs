import Control.Monad.Cont
import Control.Exception
import Data.Maybe

type Event      a = Either SomeException (Maybe a)

type Observer   a = Event a -> IO ()
type Observable a = ContT () IO (Event a)

newObservable :: (Observer a -> IO ()) -> Observable a
newObservable = ContT

subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs = newObservable $ \observer -> 
    do observer (Right (Just 1))
       observer (Right (Just 2))
       observer (Right (Nothing))

main :: IO ()
main = subscribe obs print

{-
output> 
    Right (Just 1)
    Right (Just 2)
    Right (Nothing)
-}