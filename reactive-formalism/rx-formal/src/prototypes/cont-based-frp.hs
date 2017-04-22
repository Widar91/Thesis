{-# LANGUAGE RecursiveDo, BangPatterns #-}
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Monoid
import Data.Void
import Control.Exception

newtype Event a = Event { on :: (a -> IO ()) -> IO Dispose }

newtype Dispose = Dispose { dispose :: IO () }

instance Monoid Dispose where
    mempty = Dispose (return ())
    mappend d1 d2 = Dispose $ do
        dispose d1
        dispose d2 

instance Monad Event where
    return x = Event $ \k -> k x >> return mempty
    e >>= f = Event $ \k -> do
        dref <- newIORef mempty
        addD dref e $ \x ->
            addD dref (f x) k
        return . Dispose $
            readIORef dref >>= dispose
        where
            addD :: IORef Dispose -> Event a -> (a -> IO ()) -> IO ()
            addD d e act = do
                d' <- on e act
                modifyIORef d (`mappend` d')

instance Functor Event where
    fmap = liftM

instance Applicative Event where
    pure = return
    (<*>) = ap

instance Alternative Event where
    empty = Event $ \_ -> return mempty
    e1 <|> e2 = Event $ \k -> do
        d1 <- on e1 k
        d2 <- on e2 k
        return $ d1 <> d2

instance MonadIO Event where
  liftIO m = Event $ \k -> do
    m >>= k
    return mempty

newtype Behavior a = Behavior { valueB :: IO a }

getB :: Behavior a -> Event a
getB = liftIO . valueB

apply :: Behavior (a -> b) -> Event a -> Event b
apply b e = do
  x <- e
  f <- getB b
  return $ f x

-- this breaks the whole thing!
once :: Event a -> Event a
once e = Event $ \k -> do
    rec d <- on e $ \x -> do
        dispose d `catch` (\(SomeException e) -> print "error...")
        k x
    return d

accumE :: a -> Event (a -> a) -> Event a
accumE x e = do
    f <- once e
    let !x' = f x
    pure x' <|> accumE x' e

takeE :: Int -> Event a -> Event a
takeE 0 _ = empty
takeE 1 e = once e
takeE n e | n > 1 = do
    x <- once e
    pure x <|> takeE (n - 1) e
takeE _ _ = error "takeE: n must be non-negative"

dropE :: Int -> Event a -> Event a
dropE n e = replicateM_ n (once e) >> e

logE :: Show a => Event a -> Event ()
logE e = e >>= liftIO . print

runEvent :: Event Void -> IO ()
runEvent e = void $ on e absurd

runEvent_ :: Event a -> IO ()
runEvent_ = runEvent . (>> empty)

obs = Event $ \obr -> do
    obr 1
    obr 2
    obr 3
    return $ Dispose (return ())

main = do
    on (once obs) $ \x -> print 1
    print "Done"