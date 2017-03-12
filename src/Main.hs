module Main where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import System.IO

import Data.Time.Clock.POSIX
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Control.Monad
import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent
import Gfx.LoadShaders
import Gfx.GeometryBuffers
import Gfx.Geometries

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
errorCallback err description = hPutStrLn stderr description


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
      mw <- GLFW.createWindow 400 400 "Improviz" Nothing Nothing
      maybe' mw (GLFW.terminate >> exitFailure) $ \window -> do
        GLFW.makeContextCurrent mw
        cvma <- getWindowContextVersionMajor window
        cvmi <- getWindowContextVersionMinor window
        cvr <- getWindowContextVersionRevision window
        print $ show cvma ++ ":" ++ show cvmi ++ ":" ++ show cvr
        program <- loadShaders [
            ShaderInfo VertexShader (FileSource "shaders/triangles.vert"),
            ShaderInfo FragmentShader (FileSource "shaders/triangles.frag")]
        currentProgram $= Just program
        gfxState <- baseState
        appState <- newMVar makeAppState
        _ <- forkIO $ runServer appState
        display window gfxState appState
        GLFW.destroyWindow window
        GLFW.terminate
        exitSuccess

display :: GLFW.Window -> EngineState -> MVar AppState -> IO ()
display w gfxState appState = unless' (GLFW.windowShouldClose w) $ do
  as <- readMVar appState
  Just t <- GLFW.getTime
  case fst $ L.createGfx [("time", LA.Number t)] (validAst as) of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right scene ->
      do

        (width, height) <- GLFW.getFramebufferSize w
        let ratio = fromIntegral width / fromIntegral height
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

        clearColor $= sceneBackground scene
        clear [ ColorBuffer, DepthBuffer ]

        matrixMode $= Projection
        loadIdentity
        ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
        matrixMode $= Modelview 0

        loadIdentity

        evalStateT (Gfx.interpretGfx $ Gfx.sceneGfx scene) gfxState

        GLFW.swapBuffers w
        GLFW.pollEvents
        display w gfxState appState
