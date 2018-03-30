module Main where

import           GHC.Float              (double2Float)
import           System.Exit            (exitSuccess)

import           Control.Concurrent     (forkIO)
import           Control.Concurrent.STM
import           Control.Monad          (unless, when)
import           Data.Maybe             (fromMaybe)

import           Lens.Simple            ((^.))

import qualified Graphics.UI.GLFW       as GLFW

import           AppState               (AppState, ImprovizError (..),
                                         makeAppState)
import qualified AppState               as AS
import qualified Configuration          as C
import qualified Configuration.Font     as CF
import           Improviz               (ImprovizEnv, appstate, config,
                                         createEnv, gfxstate, starttime)

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
main = createEnv >>= app

app :: ImprovizEnv -> IO ()
app env = do
  time <- double2Float . fromMaybe 0.0 <$> GLFW.getTime
  atomically $ do
    writeTVar (env ^. starttime) time
    writeTVar (env ^. appstate) (makeAppState time)
  _ <- forkIO $ runServer (env ^. appstate) (env ^. config . C.serverPort)
  setupWindow
    (env ^. config . C.screenWidth)
    (env ^. config . C.screenHeight)
    (env ^. config . C.fullscreenDisplay)
    (initApp env)
    (resize env)
    (display env)
  exitSuccess

initApp :: ImprovizEnv -> Int -> Int -> IO ()
initApp env width height =
  let ratio = fromIntegral width / fromIntegral height
      front = 0.1
      back = 100
      proj = projectionMat front back (pi / 4) ratio
      view = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
  in do post <- createPostProcessing (fromIntegral width) (fromIntegral height)
        textRenderer <-
          createTextRenderer
            front
            back
            width
            height
            (env ^. config . C.fontConfig . CF.filepath)
            (env ^. config . C.fontConfig . CF.size)
            (env ^. config . C.fontConfig . CF.fgColour)
            (env ^. config . C.fontConfig . CF.bgColour)
        textureLib <- createTextureLib (env ^. config . C.textureDirectories)
        gfxEngineState <-
          createGfxEngineState proj view post textRenderer textureLib
        atomically $ putTMVar (env ^. gfxstate) gfxEngineState

resize :: ImprovizEnv -> GLFW.WindowSizeCallback
resize env window newWidth newHeight = do
  logInfo "Resizing"
  (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
  engineState <- atomically $ readTMVar (env ^. gfxstate)
  deletePostProcessing $ postFX engineState
  newPost <- createPostProcessing (fromIntegral fbWidth) (fromIntegral fbHeight)
  newTrender <-
    resizeTextRendererScreen 0.1 100 fbWidth fbHeight (textRenderer engineState)
  let newRatio = fromIntegral fbWidth / fromIntegral fbHeight
      newProj = projectionMat 0.1 100 (pi / 4) newRatio
  atomically $ do
    es <- takeTMVar (env ^. gfxstate)
    putTMVar
      (env ^. gfxstate)
      es
      {projectionMatrix = newProj, postFX = newPost, textRenderer = newTrender}

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as <- readTVarIO (env ^. appstate)
  let t = double2Float time
      beat = AS.getBeat t as
      interpreterState =
        updateStateVariables
          [("time", Number t), ("beat", Number beat)]
          (as ^. AS.initialInterpreter)
  case createGfx interpreterState (as ^. AS.currentAst) of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $
        modifyTVar
          (env ^. appstate)
          (AS.resetProgram . AS.addError (ImprovizError msg Nothing))
    Right scene -> do
      when (AS.programHasChanged as) $ do
        logInfo "Saving current ast"
        atomically $ modifyTVar (env ^. appstate) AS.saveProgram
      gs <- atomically $ readTMVar (env ^. gfxstate)
      renderGfx gs scene
      when (as ^. AS.displayText) $ do
        renderText 0 0 (textRenderer gs) (as ^. AS.programText)
        renderTextbuffer (textRenderer gs)
