import Data.Functor.Contravariant

newtype Observer   a = Observer   { onNext :: a -> IO () } 
newtype Observable a = Observable { subscribe :: Observer a -> IO () } 

instance Contravariant Observer where 
    contramap f ob = Observer $ onNext ob . f    

instance Functor Observable where
    fmap f ooa = Observable $ subscribe ooa . contramap f
    