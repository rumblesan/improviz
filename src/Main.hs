module Main where

import GHC.Float (double2Float)

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import System.IO

import Control.Monad
import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent
import qualified Gfx.Matrices as GM

import Gfx
import qualified Language as L
import qualified Language.LanguageAst as LA
import AppServer
import AppTypes

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


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

main :: IO ()
main = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  bool successfulInit exitFailure $
    do
      v <- GLFW.getVersion
      print v
      GLFW.windowHint $ WindowHint'ContextVersionMajor 3
      GLFW.windowHint $ WindowHint'ContextVersionMinor 2
      GLFW.windowHint $ WindowHint'OpenGLForwardCompat True
      GLFW.windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
      GLFW.windowHint $ WindowHint'DepthBits 16
      let width = 640
          height = 480
          ratio = fromIntegral width / fromIntegral height
      mw <- GLFW.createWindow width height "Improviz" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
        GLFW.makeContextCurrent mw
        cvma <- getWindowContextVersionMajor window
        cvmi <- getWindowContextVersionMinor window
        cvr <- getWindowContextVersionRevision window
        print $ show cvma ++ ":" ++ show cvmi ++ ":" ++ show cvr
        depthFunc $= Just Less
        let proj = GM.projectionMat 0.1 100 (pi/4) ratio
            view = GM.viewMat (GM.vec3 0 0 10) (GM.vec3 0 0 0) (GM.vec3 0 1 0)
        gfxState <- baseState proj view >>= newMVar
        appState <- newMVar makeAppState
        GLFW.setWindowSizeCallback window $ Just (resize gfxState)
        _ <- forkIO $ runServer appState
        display window gfxState appState
        GLFW.destroyWindow window
        GLFW.terminate
        exitSuccess

resize :: MVar EngineState -> WindowSizeCallback
resize esMV _ newWidth newHeight = do
  es <- readMVar esMV
  let newRatio = fromIntegral newWidth / fromIntegral newHeight
      newProj = GM.projectionMat 0.1 100 (pi/4) newRatio
  print "Resizing"
  putMVar esMV es { projectionMatrix = newProj }

display :: GLFW.Window -> MVar EngineState -> MVar AppState -> IO ()
display w gfxState appState = unless' (GLFW.windowShouldClose w) $ do
  as <- readMVar appState
  gs <- readMVar gfxState
  Just t <- GLFW.getTime
  case fst $ L.createGfx [("time", LA.Number (double2Float t))] (validAst as) of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right scene ->
      do
        clearColor $= sceneBackground scene
        clear [ ColorBuffer, DepthBuffer ]
        evalStateT (Gfx.interpretGfx $ Gfx.sceneGfx scene) gs
        GLFW.swapBuffers w
        GLFW.pollEvents
        display w gfxState appState
