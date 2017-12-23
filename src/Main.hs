module Main where

import           GHC.Float              (double2Float)
import           System.Exit            (exitSuccess)

import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           Control.Monad          (unless, when)
import           Data.Maybe             (fromMaybe)

import           Lens.Simple            ((^.))

import qualified Graphics.UI.GLFW       as GLFW

import           AppState               (AppState, ImprovizError (..))
import qualified AppState               as AS
import           Configuration          (ImpConfig (..), ImpFontConfig (..),
                                         getConfig)
import           Gfx                    (EngineState (..), Scene (..),
                                         createGfxEngineState, renderGfx)
import           Gfx.Matrices           (projectionMat, vec3, viewMat)
import           Gfx.PostProcessing     (createPostProcessing,
                                         deletePostProcessing,
                                         renderPostProcessing,
                                         usePostProcessing)
import           Gfx.TextRendering      (TextRenderer, createTextRenderer,
                                         renderText, renderTextbuffer,
                                         resizeTextRendererScreen)
import           Gfx.Textures           (createTextureLib)
import           Gfx.Windowing          (setupWindow)
import           Language               (createGfx, updateStateVariables)
import           Language.Ast           (Value (Number))
import           Logging                (logError, logInfo)
import           Server.Http            (runServer)

main :: IO ()
main = getConfig >>= app

app :: ImpConfig -> IO ()
app cfg = do
  esVar <- newEmptyTMVarIO
  startTime <- double2Float . fromMaybe 0.0 <$> GLFW.getTime
  asTVar <- newTVarIO (AS.makeAppState startTime)
  _ <- forkIO $ runServer asTVar (serverPort cfg)
  let initialWidth = screenWidth cfg
  let initialHeight = screenHeight cfg
  let initCB = initApp esVar cfg
  let resizeCB = resize esVar
  let displayCB = display asTVar esVar
  setupWindow
    initialWidth
    initialHeight
    (fullscreenDisplay cfg)
    initCB
    resizeCB
    displayCB
  exitSuccess

initApp :: TMVar EngineState -> ImpConfig -> Int -> Int -> IO ()
initApp esVar cfg width height =
  let ratio = fromIntegral width / fromIntegral height
      front = 0.1
      back = 100
      proj = projectionMat front back (pi / 4) ratio
      view = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
      fontCfg = fontConfig cfg
  in do post <- createPostProcessing (fromIntegral width) (fromIntegral height)
        textRenderer <-
          createTextRenderer
            front
            back
            width
            height
            (fontFilePath fontCfg)
            (fontSize fontCfg)
            (fontFGColour fontCfg)
            (fontBGColour fontCfg)
        textureLib <- createTextureLib $ textureDirectories cfg
        gfxEngineState <-
          createGfxEngineState proj view post textRenderer textureLib
        atomically $ putTMVar esVar gfxEngineState

resize :: TMVar EngineState -> GLFW.WindowSizeCallback
resize esVar window newWidth newHeight = do
  logInfo "Resizing"
  (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
  engineState <- atomically $ readTMVar esVar
  deletePostProcessing $ postFX engineState
  newPost <- createPostProcessing (fromIntegral fbWidth) (fromIntegral fbHeight)
  newTrender <-
    resizeTextRendererScreen 0.1 100 fbWidth fbHeight (textRenderer engineState)
  let newRatio = fromIntegral fbWidth / fromIntegral fbHeight
      newProj = projectionMat 0.1 100 (pi / 4) newRatio
  atomically $ do
    es <- takeTMVar esVar
    putTMVar
      esVar
      es
      {projectionMatrix = newProj, postFX = newPost, textRenderer = newTrender}

display :: TVar AppState -> TMVar EngineState -> Double -> IO ()
display appState esVar time = do
  as <- readTVarIO appState
  let t = double2Float time
      beat = AS.getBeat t as
      interpreterState =
        updateStateVariables
          [("time", Number t), ("beat", Number beat)]
          (as ^. AS.initialInterpreter)
  case createGfx interpreterState (as ^. AS.currentAst) of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar appState AS.resetProgram
    Right scene -> do
      when (AS.programHasChanged as) $ do
        logInfo "Saving current ast"
        atomically $ modifyTVar appState AS.saveProgram
      gs <- atomically $ readTMVar esVar
      renderGfx gs scene
      when (as ^. AS.displayText) $ do
        renderText 0 0 (textRenderer gs) (as ^. AS.programText)
        renderTextbuffer (textRenderer gs)
