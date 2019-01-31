module Gfx.Windowing
  ( setupWindow
  ) where

import           Control.Monad             (forever)
import           Graphics.Rendering.OpenGL (ComparisonFunction (Less),
                                            depthFunc, ($=))

import qualified Graphics.UI.GLFW          as GLFW

import           Lens.Simple               ((^.))

import           System.Exit               (exitFailure)

import qualified Configuration             as C
import           Improviz                  (ImprovizEnv)
import qualified Improviz                  as I
import           Logging                   (logError, logInfo)

import           Util                      (bool, maybe', maybeElem, unless')

type InitCallback = Int -> Int -> Int -> Int -> IO ()

type DisplayCallback = Double -> IO ()

type WindowResizeCallback = Int -> Int -> Int -> Int -> IO ()

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback _ msg = logError msg

targetMonitor :: Maybe Int -> IO (Maybe GLFW.Monitor)
targetMonitor target = do
  monitors <- GLFW.getMonitors
  return $ do
    t <- target
    m <- monitors
    maybeElem t m

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

setupWindow ::
     ImprovizEnv
  -> InitCallback
  -> WindowResizeCallback
  -> DisplayCallback
  -> IO ()
setupWindow env initCB resizeCB displayCB =
  let cfg = env ^. I.config
      width = cfg ^. C.screenWidth
      height = cfg ^. C.screenHeight
      mon = cfg ^. C.fullscreenDisplay
   in do GLFW.setErrorCallback (Just errorCallback)
         successfulInit <- GLFW.init
         bool successfulInit exitFailure $ do
           GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
           GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2
           GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
           GLFW.windowHint $
             GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
           GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
           GLFW.windowHint $ GLFW.WindowHint'Decorated (cfg ^. C.decorated)
           monitor <- targetMonitor mon
           (w, h, x, y) <-
             maybe
               (return (width, height, 0, 0))
               (windowSizings width height)
               monitor
           mw <- GLFW.createWindow w h (cfg ^. C.apptitle) Nothing Nothing
           maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
             GLFW.setWindowPos window x y
             GLFW.makeContextCurrent mw
             (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
             depthFunc $= Just Less
             initCB w h fbWidth fbHeight
             GLFW.setWindowSizeCallback window $
               Just (resizeToGLFWResize resizeCB)
             logInfo $ "Improviz resolution: " ++ show w ++ " by " ++ show h
             forever $
               unless' (GLFW.windowShouldClose window) $ do
                 Just t <- GLFW.getTime
                 displayCB t
                 GLFW.swapBuffers window
                 GLFW.pollEvents
             GLFW.destroyWindow window
             GLFW.terminate
