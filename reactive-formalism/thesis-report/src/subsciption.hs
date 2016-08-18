type Subscription = IO ()

data Subscription = Subscription 
    { onUnsubscribe :: IO ()
    , subscriptions :: TMVar [Subscription] 
    }