import Control.Monad.Cont
import Graphics.UI.GLUT

type Observable a = ContT () IO a
type Observer a = a -> IO ()

observable :: (Observer a -> IO ()) -> Observable a
observable = ContT

subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs = observable $ \obr -> do
    passiveMotionCallback $= Just (\p -> obr p)    

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  subscribe obs print
  mainLoop