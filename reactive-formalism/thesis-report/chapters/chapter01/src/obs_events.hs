import Control.Monad.Cont
import Control.Exception

-- datatype representing Either Error (Maybe a)
data Event a = 
      OnNext a 
    | OnError SomeException 
    | OnCompleted

type Observable a = ContT () IO (Event a)
type Observer a = Event a -> IO ()

-- bind inherited from the Continuation monad
(>>=) :: Observable a -> (Event a -> Observable b) -> Observable b

-- bind that we would like to expose from our API
flatmap :: Observable a -> (a -> Observable b) -> Observable b

-- Monad Laws
-- return a >>= k  =  k a
-- m >>= return  =  m
-- m >>= (x -> k x >>= h)  =  (m >>= k) >>= h