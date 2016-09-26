import Control.Monad.Cont
import Control.Exception
import Data.Maybe

--   Event a = Either SomeException (Maybe a)
data Event a = OnNext a | OnError SomeException | OnCompleted
    deriving Show

type Observer   a = Event a -> IO ()
type Observable a = ContT () IO (Event a)

newObservable :: (Observer a -> IO ()) -> Observable a
newObservable = ContT

subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs = newObservable $ \observer -> 
    do observer (OnNext 1)
       observer (OnNext 2)
       observer OnCompleted

main :: IO ()
main = subscribe obs print

{-
output> 
    OnNext 1
    OnNext 1
    OnCompleted
-}