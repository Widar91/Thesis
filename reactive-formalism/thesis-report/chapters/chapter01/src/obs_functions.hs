-- Simply wraps the function :: (a -> IO ()) -> IO () 
-- inside the Observable datatype
observable :: (Observer a -> IO ()) -> Observable a
observable = ContT

-- Runs the Observable by providing the continuation - i.e. the Observer - 
-- that will handle the asynchronous data.
subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 