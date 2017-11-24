module Main where

import           GHC.Float                  (double2Float)

import           Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW           as GLFW
import           System.Exit
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State.Strict (evalStateT)
import           Data.Maybe                 (fromMaybe)
import qualified Gfx.Matrices               as GM

import           AppServer
import           AppTypes
import           Configuration              (ImpConfig (..), ImpFontConfig (..),
                                             getConfig)
import           Gfx
import           Gfx.PostProcessing
import           Gfx.TextRendering
import           Gfx.Textures               (createTextureLib)
import           Gfx.Windowing
import qualified Language                   as L
import qualified Language.Ast               as LA

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

main :: IO ()
main = getConfig >>= app

app :: ImpConfig -> IO ()
app cfg = do
  when (debug cfg) (print cfg)
  gfxETMVar <- newEmptyTMVarIO
  asTVar <- newTVarIO makeAppState
  _ <- forkIO $ runServer asTVar
  let initialWidth = screenWidth cfg
  let initialHeight = screenHeight cfg
  let initCB = initApp gfxETMVar cfg
  let resizeCB = resize gfxETMVar
  let displayCB = display asTVar gfxETMVar
  setupWindow
    initialWidth
    initialHeight
    (fullscreenDisplay cfg)
    initCB
    resizeCB
    displayCB
  exitSuccess

initApp :: TMVar EngineState -> ImpConfig -> Int -> Int -> IO ()
initApp gfxEngineTMVar cfg width height = do
  let ratio = fromIntegral width / fromIntegral height
      front = 0.1
      back = 100
      proj = GM.projectionMat front back (pi / 4) ratio
      view = GM.viewMat (GM.vec3 0 0 10) (GM.vec3 0 0 0) (GM.vec3 0 1 0)
  post <- createPostProcessing (fromIntegral width) (fromIntegral height)
  textRenderer <-
    createTextRenderer
      front
      back
      width
      height
      (fontFilePath $ fontConfig cfg)
      (fontSize $ fontConfig cfg)
      (fontFGColour $ fontConfig cfg)
      (fontBGColour $ fontConfig cfg)
  textureLib <- createTextureLib $ textureDirectories cfg
  gfxEngineState <- baseState proj view post textRenderer textureLib
  atomically $ putTMVar gfxEngineTMVar gfxEngineState

resize :: TMVar EngineState -> GLFW.WindowSizeCallback
resize esVar window newWidth newHeight = do
  print "Resizing"
  (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
  engineState <- atomically $ readTMVar esVar
  deletePostProcessing $ postFX engineState
  newPost <- createPostProcessing (fromIntegral fbWidth) (fromIntegral fbHeight)
  newTrender <-
    resizeTextRendererScreen 0.1 100 fbWidth fbHeight (textRenderer engineState)
  let newRatio = fromIntegral fbWidth / fromIntegral fbHeight
      newProj = GM.projectionMat 0.1 100 (pi / 4) newRatio
  atomically $ do
    es <- takeTMVar esVar
    putTMVar
      esVar
      es
      {projectionMatrix = newProj, postFX = newPost, textRenderer = newTrender}

display :: TVar AppState -> TMVar EngineState -> Double -> IO ()
display appState gfxState time = do
  as <- readTVarIO appState
  gs <- atomically $ readTMVar gfxState
  let vars = [("time", LA.Number (double2Float time))]
  case fst $ L.createGfx vars (currentAst as) of
    Left msg -> do
      putStrLn $ "Could not interpret program: " ++ msg
      atomically $
        modifyTVar appState (\as -> as {currentAst = lastWorkingAst as})
    Right scene -> do
      drawScene gs scene
      when (displayText as) $ drawText gs as
      unless (currentAst as == lastWorkingAst as) $ do
        putStrLn "Saving current ast"
        atomically $
          modifyTVar appState (\as -> as {lastWorkingAst = currentAst as})

drawText :: EngineState -> AppState -> IO ()
drawText es appState = do
  renderText 0 0 (textRenderer es) (programText appState)
  renderTextbuffer (textRenderer es)

beforeRender :: Scene -> IO ()
beforeRender scene = do
  let animStyle = scenePostProcessingFX scene
  frontFace $= CCW
  --cullFace $= Just Back
  depthFunc $= Just Less
  blend $= Enabled
  blendEquationSeparate $= (FuncAdd, FuncAdd)
  case animStyle of
    NormalStyle ->
      blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, One))
    MotionBlur -> do
      blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
    PaintOver -> do
      blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
  clearColor $= sceneBackground scene
  clear [ColorBuffer, DepthBuffer]

drawScene :: EngineState -> Scene -> IO ()
drawScene gs scene = do
  let post = postFX gs
  usePostProcessing post
  beforeRender scene
  evalStateT (Gfx.interpretGfx $ Gfx.sceneGfx scene) gs
  renderPostProcessing post $ scenePostProcessingFX scene
