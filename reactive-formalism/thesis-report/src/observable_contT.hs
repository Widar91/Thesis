import Control.Monad.Cont

type Observer   a = a -> IO ()
type Observable a = ContT () IO a

-- Simply wraps the function :: (a -> IO ()) -> IO () 
-- inside the Observable datatype
newObservable :: (Observer a -> IO ()) -> Observable a
newObservable = ContT

-- Runs the Observable by providing the continuation - i.e. the Observer - 
-- that will handle the asynchronous data.
subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs = newObservable $ \observer -> 
    do observer 1
       observer 2
       observer 3

main :: IO ()
main = subscribe obs print

{-
output> 
    1
    2
    3
-}
