import Data.Functor.Contravariant

{-
    type Observer a = a -> IO ()

    contramap :: (a -> b) -> Observer b -> Observer a
    contramap f ob = ob . f

    identity:
          contramap id 
        = \ob -> contramap id ob
        = \ob -> ob . id
        = \ob -> ob
        = id
        
    composition:
          (contramap p) . (contramap q)
        = \ob -> ((contramap p) . (contramap q)) ob
        = \ob -> contramap p (contramap q ob)
        = \ob -> contramap p (ob . q)
        = \ob -> (ob . q) . p
        = \ob -> ob . (q . p)
        = \ob -> contramap (q . p) ob
        = contramap (q . p)
-}

newtype Observer   a = Observer   { onNext :: a -> IO () } 

instance Contravariant Observer where 
    contramap f ob = Observer $ onNext ob . f    

{-
    type Observable a = Observer a -> IO ()

    fmap :: (a -> b) -> Observable a -> Observable b
    fmap f ooa = \ob -> ooa (contramap f ob)

    identity:
          fmap id 
        = \ooa -> fmap id ooa 
        = \ooa -> \ob -> ooa (contramap id ob)
        = \ooa -> \ob -> ooa ob
        = \ooa -> ooa
        = id

    composition:
          fmap p . fmap q
        = \ooa -> (fmap p . fmap q) ooa
        = \ooa -> fmap p (fmap q ooa)
        = \ooa -> fmap p (\ob -> ooa (contramap q ob))
        = \ooa -> \oc -> (\ob -> ooa (contramap q ob)) (contramap p oc)
        = \ooa -> \oc -> ooa (contramap q (contramap p oc))
        = \ooa -> \oc -> ooa ((contramap q . contramap p)  oc)
        = \ooa -> \oc -> ooa (contramap (p . q) oc)
        = \ooa -> fmap (p . q) ooa
        = fmap (p . q)
-}

newtype Observable a = Observable { subscribe :: Observer a -> IO () } 

instance Functor Observable where
    fmap f ooa = Observable $ subscribe ooa . contramap f