module Main where

import GHC.Float (double2Float)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import System.IO

import Control.Monad
import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Gfx.Matrices as GM

import Gfx
import qualified Language as L
import qualified Language.LanguageAst as LA
import AppServer
import AppTypes
import Gfx.PostProcessing
import Gfx.Windowing


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

main :: IO ()
main = do
  gfxETMVar <- newEmptyTMVarIO
  asTVar <- newTVarIO makeAppState
  _ <- forkIO $ runServer asTVar

  let initialWidth = 640
  let initialHeight = 480
  let initCB = initApp gfxETMVar
  let resizeCB = resize gfxETMVar
  let displayCB = display asTVar gfxETMVar
  setupWindow initialWidth initialHeight initCB resizeCB displayCB
  exitSuccess

initApp :: TMVar EngineState -> Int -> Int -> IO ()
initApp gfxEngineTMVar width height = do
  let ratio = fromIntegral width / fromIntegral height
      proj = GM.projectionMat 0.1 100 (pi/4) ratio
      view = GM.viewMat (GM.vec3 0 0 10) (GM.vec3 0 0 0) (GM.vec3 0 1 0)

  post <- createPostProcessing (fromIntegral width) (fromIntegral height)

  gfxEngineState <- baseState proj view post
  atomically$ putTMVar gfxEngineTMVar gfxEngineState


resize :: TMVar EngineState -> GLFW.WindowSizeCallback
resize var _ newWidth newHeight = do
  print "Resizing"
  let newRatio = fromIntegral newWidth / fromIntegral newHeight
      newProj = GM.projectionMat 0.1 100 (pi/4) newRatio
  atomically $ do
    es <- takeTMVar var
    putTMVar var es { projectionMatrix = newProj }

display :: TVar AppState -> TMVar EngineState -> Double -> IO ()
display appState gfxState time = do
  as <- readTVarIO appState
  gs <- atomically $ readTMVar gfxState
  let vars = [("time", LA.Number (double2Float time))]

  case fst $ L.createGfx vars (currentAst as) of
    Left msg -> do
      putStrLn $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar appState (\as -> as { currentAst = lastWorkingAst as })
    Right scene ->
      do
        drawScene gs scene
        unless (currentAst as == lastWorkingAst as) $ do
          putStrLn "Saving current ast"
          atomically $ modifyTVar appState (\as -> as { lastWorkingAst = currentAst as })

drawScene :: EngineState -> Scene -> IO ()
drawScene gs scene =
  do
    let post = postFX gs
    usePostProcessing post

    depthFunc $= Just Less
    clearColor $= sceneBackground scene
    clear [ ColorBuffer, DepthBuffer ]
    evalStateT (Gfx.interpretGfx $ Gfx.sceneGfx scene) gs

    renderPostProcessing post NormalStyle

