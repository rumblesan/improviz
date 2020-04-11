module Windowing
  ( setupWindow
  )
where

import           Control.Monad                  ( forever )
import           Graphics.Rendering.OpenGL      ( ComparisonFunction(Less)
                                                , depthFunc
                                                , ($=)
                                                )

import qualified Graphics.UI.GLFW              as GLFW

import           Lens.Simple                    ( (^.) )
import           Safe                           ( atMay )

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C
import           Improviz                       ( ImprovizEnv )
import           Logging                        ( logError
                                                , logInfo
                                                )

import           Util                           ( bool
                                                , maybe'
                                                , unless'
                                                )

type InitCallback = Int -> Int -> Int -> Int -> IO ImprovizEnv

type DisplayCallback = ImprovizEnv -> Double -> IO ()

type WindowResizeCallback = Int -> Int -> Int -> Int -> IO ()

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback _ = logError

targetMonitor :: Maybe Int -> IO (Maybe GLFW.Monitor)
targetMonitor target = do
  monitors <- GLFW.getMonitors
  return $ do
    t <- target
    m <- monitors
    atMay m t

screenSize :: GLFW.VideoMode -> (Int, Int)
screenSize videoMode =
  (GLFW.videoModeWidth videoMode, GLFW.videoModeHeight videoMode)

windowSizings :: Int -> Int -> GLFW.Monitor -> IO (Int, Int, Int, Int)
windowSizings defWidth defHeight mon = do
  videoMode <- GLFW.getVideoMode mon
  let (w, h) = maybe (defWidth, defHeight) screenSize videoMode
  (x, y) <- GLFW.getMonitorPos mon
  return (w, h, x, y)

resizeToGLFWResize :: WindowResizeCallback -> GLFW.WindowSizeCallback
resizeToGLFWResize cb window newWidth newHeight = do
  (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
  cb newWidth newHeight fbWidth fbHeight

windowCloseCB :: GLFW.Window -> IO ()
windowCloseCB w = logInfo "Closing" >> exitSuccess

setupWindow
  :: ImprovizConfig
  -> InitCallback
  -> (ImprovizEnv -> WindowResizeCallback)
  -> DisplayCallback
  -> IO ()
setupWindow config initCB resizeCB displayCB =
  let
    width  = config ^. C.screenWidth
    height = config ^. C.screenHeight
    mon    = config ^. C.fullscreenDisplay
  in
    do
      GLFW.setErrorCallback (Just errorCallback)
      successfulInit <- GLFW.init
      bool successfulInit exitFailure $ do
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
        GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
        GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
        GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
        GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
        GLFW.windowHint $ GLFW.WindowHint'Decorated (config ^. C.decorated)
        monitor      <- targetMonitor mon
        (w, h, x, y) <- maybe (return (width, height, 0, 0))
                              (windowSizings width height)
                              monitor
        mw <- GLFW.createWindow w h (config ^. C.apptitle) Nothing Nothing
        maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
          GLFW.setWindowPos window (x + 5) (y + 5) -- horrible hack for OSX 10.14
          GLFW.makeContextCurrent mw
          (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
          depthFunc $= Just Less
          env <- initCB w h fbWidth fbHeight
          GLFW.setWindowCloseCallback window $ Just windowCloseCB
          GLFW.setWindowSizeCallback window
            $ Just (resizeToGLFWResize $ resizeCB env)
          logInfo $ "Improviz resolution: " ++ show w ++ " by " ++ show h
          -- next 3 lines also for horrible hack
          -- otherwise the screen is just black and won't draw
          -- https://github.com/glfw/glfw/issues/1334
          GLFW.swapBuffers window
          GLFW.pollEvents
          GLFW.setWindowPos window x y
          forever $ unless' (GLFW.windowShouldClose window) $ do
            Just t <- GLFW.getTime
            displayCB env t
            GLFW.swapBuffers window
            GLFW.pollEvents
          GLFW.destroyWindow window
          GLFW.terminate
