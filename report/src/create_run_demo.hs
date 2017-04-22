import Control.Monad.Cont
import Control.Exception
import Graphics.UI.GLUT

data Event a = OnNext a | OnError SomeException | OnCompleted
    deriving Show

type Observer   a = Event a -> IO ()
type Observable a = ContT () IO (Event a)

newObservable :: (Observer a -> IO ()) -> Observable a
newObservable = ContT

subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs :: Observable Char
obs = newObservable $ \observer -> do
    keyboardCallback $= Just (\c p -> observer (OnNext c))    

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Observable Keyboard"
  subscribe obs (print . show)
  displayCallback $= display
  mainLoop
