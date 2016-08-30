{-

f   ::  a ->  b
     = -a -> +b

g   ::  ( a ->  b) ->  c
     = -(-a -> +b) -> +c
     =  (+a -> -b) -> +c

observer
    ::  a -> ()
     = -a -> ()

observable
    ::  ( a -> ()) -> ()
     = -(-a -> ()) -> ()
     =  (+a -> ()) -> ()

-}

randomValueObs :: Observable Int
randomValueObs = Observable $ \observer -> do
    int <- randomRIO (1, 10) 
    observer int