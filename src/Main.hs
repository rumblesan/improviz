module Main where

import GHC.Float (double2Float)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import System.IO

import Control.Monad.State.Strict (evalStateT)
import Control.Concurrent
import qualified Gfx.Matrices as GM

import Gfx
import qualified Language as L
import qualified Language.LanguageAst as LA
import AppServer
import AppTypes
import Gfx.PostProcessing
import Gfx.GeometryBuffers (VBO(..))
import Gfx.Windowing


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

main :: IO ()
main = do
  gfxEMVar <- newEmptyMVar
  asMVar <- newEmptyMVar

  let initialWidth = 640
  let initialHeight = 480
  let initCB = initApp gfxEMVar asMVar
  let resizeCB = resize gfxEMVar
  let displayCB = display gfxEMVar asMVar
  setupWindow initialWidth initialHeight initCB resizeCB displayCB
  exitSuccess

initApp :: MVar EngineState -> MVar AppState -> Int -> Int -> IO ()
initApp gfxEngineMVar appStateMVar width height = do
  let ratio = fromIntegral width / fromIntegral height
      proj = GM.projectionMat 0.1 100 (pi/4) ratio
      view = GM.viewMat (GM.vec3 0 0 10) (GM.vec3 0 0 0) (GM.vec3 0 1 0)
  post <- createPostProcessing (fromIntegral width) (fromIntegral height)
  _ <- forkIO $ runServer appStateMVar
  gfxEngineState <- baseState proj view post
  putMVar gfxEngineMVar gfxEngineState
  putMVar appStateMVar makeAppState


resize :: MVar EngineState -> GLFW.WindowSizeCallback
resize esMV _ newWidth newHeight = do
  es <- readMVar esMV
  let newRatio = fromIntegral newWidth / fromIntegral newHeight
      newProj = GM.projectionMat 0.1 100 (pi/4) newRatio
  print "Resizing"
  putMVar esMV es { projectionMatrix = newProj }

display :: MVar EngineState -> MVar AppState -> Double -> IO ()
display gfxState appState time = do
  as <- readMVar appState
  gs <- readMVar gfxState
  let vars = [("time", LA.Number (double2Float time))]

  case fst $ L.createGfx vars (currentAst as) of
    Left msg -> putStrLn $ "Could not interpret program: " ++ msg
    Right scene -> drawScene gs scene

drawScene :: EngineState -> Scene -> IO ()
drawScene gs scene =
  do
    let post = postFX gs
    bindFramebuffer Framebuffer $= frameBuffer post
    clearColor $= sceneBackground scene
    clear [ ColorBuffer, DepthBuffer ]
    evalStateT (Gfx.interpretGfx $ Gfx.sceneGfx scene) gs

    bindFramebuffer Framebuffer $= (defaultFrameBuffer post)
    clear [ ColorBuffer, DepthBuffer ]
    currentProgram $= Just (postShaders post)

    let (VBO qbo qbai qbn) = renderQuadVBO post
    bindVertexArrayObject $= Just qbo

    drawArrays Triangles qbai qbn


