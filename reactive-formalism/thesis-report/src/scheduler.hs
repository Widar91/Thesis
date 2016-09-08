import System.IO
import Control.Monad
import Data.Time
import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Tiempo
import Data.Functor.Contravariant

newtype Observable a = Observable 
    { subscribe :: Observer a -> IO () 
    }

data Observer a = Observer 
    { onNext       :: a -> IO ()
    , onError      :: SomeException -> IO ()
    , onCompleted  :: IO ()
    }

instance Contravariant Observer where 
    contramap f ob = Observer (onNext ob . f) (onError ob) (onCompleted ob)    

instance Functor Observable where
    fmap f ooa = Observable $ subscribe ooa . contramap f

lift :: Observable a -> (Observer b -> Observer a) -> Observable b
lift ooa f = Observable $ \ob -> subscribe ooa (f ob)

-------------------------------------------------------------------------------

data Scheduler = Scheduler 
    { _schedule      :: IO () -> IO ()
    , _scheduleDelay :: TimeInterval -> IO () -> IO ()
    }

newThread :: IO Scheduler
newThread = do
    ch  <- newTChanIO
    tid <- forkIO $ forever $ do
        join $ atomically $ readTChan ch
        yield
    return $ Scheduler (schedule_ ch) (scheduleDelay_ ch)
        where
            schedule_ ch io = atomically $ writeTChan ch io
            scheduleDelay_ ch delay io = do
                threadDelay $ toMicroSeconds delay
                atomically $ writeTChan ch io

observeOn :: Observable a -> IO Scheduler -> Observable a
observeOn o sched = Observable $ \obr -> do
    s <- sched
    subscribe o (f s obr)
        where
            f s downstream = Observer 
                {   onNext      = void . _schedule s . onNext downstream
                ,   onError     = void . _schedule s . onError downstream
                ,   onCompleted = void . _schedule s $ onCompleted downstream
                }

obs = Observable $ \obr -> 
    do onNext obr 1
       onNext obr 2
       onNext obr 3
       onCompleted obr

obr :: Observer Int
obr = Observer on oe oc
    where 
        on v = do
            tid <- myThreadId 
            print (show tid ++ ": " ++ show v)
        oe = print . show
        oc = print "Completed"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering  
    subscribe obs' obr
    tid <- myThreadId 
    putStrLn $ "MainThreadId: " ++ show tid
    where 
        obs'  = obs `rxmap` (+1) `observeOn` newThread `rxmap` (+10)
        rxmap = flip fmap

{-
output> 
    ThreadId 2: 12
    ThreadId 2: 13
    ThreadId 2: 14
    Completed
    MainThreadId: 1
-}