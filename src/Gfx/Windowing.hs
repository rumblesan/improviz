module Gfx.Windowing where

import           Control.Monad
import           Data.Maybe                (fromMaybe)
import           ErrorHandling             (glfwErrorCallback)
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW          as GLFW

import           Lens.Simple               ((^.))

import           System.Exit
import           System.IO

import qualified Configuration             as C
import           Improviz                  (ImprovizEnv)
import qualified Improviz                  as I

bool :: Bool -> a -> a -> a
bool b falseRes trueRes =
  if b
    then trueRes
    else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
  b <- action
  unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f =
  case m of
    Nothing -> nothingRes
    Just x  -> f x

maybeElem :: Int -> [a] -> Maybe a
maybeElem _ []     = Nothing
maybeElem 0 (x:xs) = Just x
maybeElem i (_:xs) = maybeElem (i - 1) xs

type InitCallback = Int -> Int -> IO ()

type DisplayCallback = Double -> IO ()

targetMonitor :: Maybe Int -> IO (Maybe Monitor)
targetMonitor target = do
  monitors <- GLFW.getMonitors
  return $ do
    t <- target
    m <- monitors
    maybeElem t m

screenSize :: VideoMode -> (Int, Int)
screenSize videoMode =
  (GLFW.videoModeWidth videoMode, GLFW.videoModeHeight videoMode)

windowSizings :: Int -> Int -> Monitor -> IO (Int, Int, Int, Int)
windowSizings defWidth defHeight mon = do
  videoMode <- GLFW.getVideoMode mon
  let (w, h) = maybe (defWidth, defHeight) screenSize videoMode
  (x, y) <- getMonitorPos mon
  return (w, h, x, y)

setupWindow ::
     ImprovizEnv
  -> InitCallback
  -> WindowSizeCallback
  -> DisplayCallback
  -> IO ()
setupWindow env initCB resizeCB displayCB =
  let cfg = env ^. I.config
      width = cfg ^. C.screenWidth
      height = cfg ^. C.screenHeight
      mon = cfg ^. C.fullscreenDisplay
      ratio = fromIntegral width / fromIntegral height
   in do GLFW.setErrorCallback (Just glfwErrorCallback)
         successfulInit <- GLFW.init
         bool successfulInit exitFailure $ do
           GLFW.windowHint $ WindowHint'ContextVersionMajor 3
           GLFW.windowHint $ WindowHint'ContextVersionMinor 2
           GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
           GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
           GLFW.windowHint $ WindowHint'DepthBits 16
           GLFW.windowHint $ WindowHint'Decorated (cfg ^. C.decorated)
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
             initCB fbWidth fbHeight
             GLFW.setWindowSizeCallback window $ Just resizeCB
             forever $
               unless' (GLFW.windowShouldClose window) $ do
                 Just t <- GLFW.getTime
                 displayCB t
                 GLFW.swapBuffers window
                 GLFW.pollEvents
             GLFW.destroyWindow window
             GLFW.terminate
