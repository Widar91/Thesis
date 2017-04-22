import Control.Monad


newtype Iterator a = Iterator 
    { moveNext :: () -> IO a 
    } 

instance Functor Iterator where
    fmap f ia  = Iterator $ \() -> liftM f (moveNext ia ()) 


newtype Iterable a = Iterable 
    { getIterator :: () -> IO (Iterator a) 
    } 

instance Functor Iterable where
    fmap f iia = Iterable $ \() -> liftM (fmap f) (getIterator iia ())


    -- Using a type synonym instead of Haskell's newtypes, 
    -- in order to avoid clutter in our proofs:

    type Iterator a = () -> IO a

    fmap :: (a -> b) -> Iterator a -> Iterator b
    fmap f ia = \() -> ia () >>= return . f

    -- identity:
          fmap id                                        
            -- eta abstraction 
        = \ia -> fmap id ia                              
            -- definition of fmap 
        = \ia -> \() -> ia () >>= return . id            
            -- application of id
        = \ia -> \() -> ia () >>= return                 
            -- IO monad right identity* 
        = \ia -> \() -> ia ()                            
            -- eta reduction
        = \ia -> ia                                      
            -- definition of
        = id

    -- composition:
          (fmap p) . (fmap q)                                                              
            -- eta abstraction
        = \ia -> ((fmap p) . (fmap q)) ia                                                  
            -- definition of (.)*
        = \ia -> fmap p (fmap q ia)                                                        
            -- definition of fmap q
        = \ia -> fmap p (\() -> ia () >>= return . q)                                      
            -- definition of fmap p
        = \ia -> \() -> (\() -> ia () >>= return . q) () >>= return . p                    
            -- eta reduction inner lambda
        = \ia -> \() -> ia () >>= return . q >>= return . p                                
            -- eta abstraction
        = \ia -> \() -> ia () >>= \a -> (return . q) a >>= return . p                      
            -- definition of (.)
        = \ia -> \() -> ia () >>= \a -> return (q a) >>= return . p                        
            -- IO monad left identity*
        = \ia -> \() -> ia () >>= \a -> (return . p) (q a)                                 
            -- definition of (.)   
        = \ia -> \() -> ia () >>= \a -> (return . p . q) a                                 
            -- definition of (.) 
        = \ia -> \() -> ia () >>= \a -> return ((p . q) a)                                 
            -- definition of fmap
        = \ia -> fmap (p . q) ia                                                           
            -- eta reduction
        = fmap (p . q)
      
    -- * monad right identity:
            m >>= return = m
      
    --   monad left identity:
            return a >>= f = f a  
      
    --  defintion of (.): 
            (.) :: (b -> c) -> (a -> b) -> a -> c
            (f . g) a = f (g a) 

    ---------------------------------------------------------------------------

    type Iterable a = () -> IO (Iterator a)

    fmap :: (a -> b) -> Iterable a -> Iterable b
    fmap f iia = \() -> iia () >>= return . fmap f

    -- identity:
          fmap id                                       
            -- eta abstraction 
        = \iia -> fmap id iia                           
            -- definition of fmap 
        = \iia -> \() -> iia () >>= return . fmap id    
            -- Iterator identity law
        = \iia -> \() -> iia () >>= return . id         
            -- application of id
        = \iia -> \() -> iia () >>= return              
            -- IO monad right identity  
        = \iia -> \() -> iia ()                         
            -- eta reduction   
        = \iia -> iia                                   
            -- definition of id
        = id

    -- composition:
          (fmap p) . (fmap q)                                                          
            -- eta abstraction  
        = \iia -> ((fmap p) . (fmap q)) iia                                            
            -- definition of (.)
        = \iia -> fmap p (fmap q iia)                                                  
            -- definition of fmap
        = \iia -> fmap p (\() -> iia () >>= return . fmap q)                           
            -- definition of fmap
        = \iia -> \() -> (\() -> iia () >>= return . fmap q) () >>= return . fmap p    
            -- eta reduction
        = \iia -> \() -> iia () >>= return . fmap q >>= return . fmap p                
            -- eta abstraction  
        = \iia -> \() -> iia () >>= \ia -> (return . fmap q) ia >>= return . fmap p    
            -- definition of (.)
        = \iia -> \() -> iia () >>= \ia -> return (fmap q ia) >>= return . fmap p      
            -- IO monad left identity
        = \iia -> \() -> iia () >>= \ia -> (return . fmap p) (fmap q ia)               
            -- definition of (.)
        = \iia -> \() -> iia () >>= \ia -> (return . fmap p . fmap q) ia               
            -- eta reduction 
        = \iia -> \() -> iia () >>= return . fmap p . fmap q                           
            -- Iterator composition law
        = \iia -> \() -> iia () >>= return . fmap (p . q)                              
            -- definiton of fmap 
        = \iia -> fmap (p . q) iia                                                     
            -- eta reduction   
        = fmap (p . q)