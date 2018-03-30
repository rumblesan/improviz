module Main where

import           GHC.Float              (double2Float)
import           System.Exit            (exitSuccess)

import           Control.Concurrent.STM (atomically, modifyTVar, putTMVar,
                                         readTMVar, readTVarIO, takeTMVar,
                                         writeTVar)
import           Control.Monad          (when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader   (ask, runReader)
import           Data.Maybe             (fromMaybe)

import           Lens.Simple            ((^.))

import qualified Graphics.UI.GLFW       as GLFW

import           AppState               (ImprovizError (..))
import qualified AppState               as AS
import qualified Configuration          as C
import qualified Configuration.Font     as CF
import           Improviz               (ImprovizApp, ImprovizEnv)
import qualified Improviz               as I

import           Gfx                    (EngineState (..), createGfxEngineState,
                                         renderGfx)
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
import           Server.Http            (createServer)

main :: IO ()
main = do
  starttime <- double2Float . fromMaybe 0.0 <$> GLFW.getTime
  env <- I.createEnv starttime
  runReader app env

app :: (MonadIO m) => ImprovizApp (m ())
app = do
  initCallback <- initApp
  resizeCallback <- resize
  displayCallback <- display
  server <- createServer
  window <- setupWindow initCallback resizeCallback displayCallback
  return $ do
    server
    window

initApp :: ImprovizApp (Int -> Int -> IO ())
initApp = do
  env <- ask
  return
    (\width height -> do
       let ratio = fromIntegral width / fromIntegral height
       let front = 0.1
       let back = 100
       let proj = projectionMat front back (pi / 4) ratio
       let view = viewMat (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
       post <- createPostProcessing (fromIntegral width) (fromIntegral height)
       textRenderer <-
         createTextRenderer (env ^. I.config) front back width height
       textureLib <- createTextureLib (env ^. I.config . C.textureDirectories)
       gfxEngineState <-
         createGfxEngineState proj view post textRenderer textureLib
       atomically $ putTMVar (env ^. I.gfxstate) gfxEngineState)

resize :: ImprovizApp GLFW.WindowSizeCallback
resize = do
  env <- ask
  return $ \window newWidth newHeight -> do
    logInfo "Resizing"
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize window
    engineState <- atomically $ readTMVar (env ^. I.gfxstate)
    deletePostProcessing $ postFX engineState
    newPost <-
      createPostProcessing (fromIntegral fbWidth) (fromIntegral fbHeight)
    newTrender <-
      resizeTextRendererScreen
        0.1
        100
        fbWidth
        fbHeight
        (textRenderer engineState)
    let newRatio = fromIntegral fbWidth / fromIntegral fbHeight
        newProj = projectionMat 0.1 100 (pi / 4) newRatio
    atomically $ do
      es <- takeTMVar (env ^. I.gfxstate)
      putTMVar
        (env ^. I.gfxstate)
        es
        { projectionMatrix = newProj
        , postFX = newPost
        , textRenderer = newTrender
        }

display :: ImprovizApp (Double -> IO ())
display = do
  env <- ask
  return
    (\time -> do
       as <- readTVarIO (env ^. I.appstate)
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
               (env ^. I.appstate)
               (AS.resetProgram . AS.addError (ImprovizError msg Nothing))
         Right scene -> do
           when (AS.programHasChanged as) $ do
             logInfo "Saving current ast"
             atomically $ modifyTVar (env ^. I.appstate) AS.saveProgram
           gs <- atomically $ readTMVar (env ^. I.gfxstate)
           renderGfx gs scene
           when (as ^. AS.displayText) $ do
             renderText 0 0 (textRenderer gs) (as ^. AS.programText)
             renderTextbuffer (textRenderer gs))
