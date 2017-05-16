module Gfx.Windowing where

import Data.Maybe (fromMaybe)
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import Control.Monad

import System.Exit
import System.IO

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x

maybeElem :: Int -> [a] -> Maybe a
maybeElem _ [] = Nothing
maybeElem 0 (x:xs) = Just x
maybeElem i (_:xs) = maybeElem (i - 1) xs

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

type InitCallback = Int -> Int -> IO ()
type DisplayCallback = Double -> IO ()

targetMonitor :: Maybe Int -> IO (Maybe Monitor)
targetMonitor target =
  do
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
  let (w, h) = fromMaybe (defWidth, defHeight) $ screenSize <$> videoMode
  (x, y) <- getMonitorPos mon
  return (w, h, x, y)

setupWindow :: Int -> Int -> Maybe Int -> InitCallback -> WindowSizeCallback -> DisplayCallback -> IO ()
setupWindow width height mon initCB resizeCB displayCB = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  bool successfulInit exitFailure $
    do
      GLFW.windowHint $ WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ WindowHint'ContextVersionMinor 2
      GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
      GLFW.windowHint $ WindowHint'DepthBits 16
      GLFW.windowHint $ WindowHint'Decorated False
      let ratio = fromIntegral width / fromIntegral height
      monitor <- targetMonitor mon

      (w, h, x, y) <- fromMaybe (return (width, height, 0, 0)) $ windowSizings width height <$> monitor

      mw <- GLFW.createWindow w h "Improviz" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
        GLFW.setWindowPos window x y
        GLFW.makeContextCurrent mw
        (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
        depthFunc $= Just Less
        initCB fbWidth fbHeight
        GLFW.setWindowSizeCallback window $ Just resizeCB
        forever $ unless' (GLFW.windowShouldClose window) $ do
              Just t <- GLFW.getTime
              displayCB t
              GLFW.swapBuffers window
              GLFW.pollEvents

        GLFW.destroyWindow window
        GLFW.terminate


