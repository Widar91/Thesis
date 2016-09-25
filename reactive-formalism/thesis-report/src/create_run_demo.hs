import Control.Monad.Cont
import Graphics.UI.GLUT

type Observable a = ContT () IO a
type Observer a = a -> IO ()

observable :: (Observer a -> IO ()) -> Observable a
observable = ContT

subscribe :: Observable a -> Observer a -> IO ()
subscribe = runContT 

obs :: Observable Char
obs = observable $ \obr -> do
    keyboardCallback $= Just (\c p -> obr c)    

display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  flush

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Observable Keyboard"
  subscribe obs print
  displayCallback $= display
  mainLoop
