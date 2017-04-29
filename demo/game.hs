{-# LANGUAGE OverloadedStrings #-}
module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit
import Rx

keyboardObservable :: HG3D -> IO (Observable KeyEvent)
keyboardObservable hg3d  = do 
    ieh <- newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent] 
    return . observableCreate $ \obr -> registerCallback hg3d ieh ctKeyEvent (onNext obr)

gameLogic hg3d = do

    -- create minimum elements, like a camera
    eCam <- newE hg3d [
        ctCamera #: FullViewCamera,
        ctPosition #: Vec3 1 1 (-30.0),
        ctLight #: Light PointLight 1.0 1000.0 1.0 
        ]

    -- do something interesting here, in this example case, it is a text and
    -- a rotating cube

    eText <- newE hg3d [
        ctText #: "Rotating Cube Example",
        ctScreenRect #: Rectangle 10 10 100 25
        ]

    eGeo <- newE hg3d [
        ctGeometry #: ShapeGeometry Cube,
        ctMaterial #: matBlue,
        ctScale #: Vec3 10.0 10.0 10.0,
        ctPosition #: Vec3 0.0 0.0 0.0,
        ctOrientation #: unitU
        ]

    let rotateCube = do
            forever $ do 
                updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
                updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
                sleepFor (msecT 12)

    let observeKeyboard = do
            obs <- keyboardObservable hg3d
            obr <- createObserver onNext_ (print . show) (print "FINISHED")
            subscribe obs obr
            return ()
            where 
                onNext_ ke = do 
                    case ke of 
                        (KeyUp _ _ k) -> do
                            updateC eGeo ctOrientation (\u -> (rotU vec3Z 0.02) .*. u)
                            updateC eGeo ctOrientation (\u -> (rotU vec3X 0.015) .*. u)
                            return ()
                        _ -> return ()

    forkIO observeKeyboard
    return ()

main = do 
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()
