module Main where

import           GHC.Float              (double2Float)

import           Control.Concurrent.STM (atomically, modifyTVar, putTMVar,
                                         readTMVar, readTVarIO, takeTMVar)
import           Control.Monad          (when)
import qualified Data.Map.Strict        as M

import           Lens.Simple            ((^.))

import qualified Configuration          as C
import           Improviz               (ImprovizEnv, ImprovizError (..))
import qualified Improviz               as I
import qualified Improviz.Language      as IL
import qualified Improviz.UI            as IUI

import           Gfx                    (EngineState (..), createGfxEngineState,
                                         renderGfx, resizeGfxEngine)
import           Gfx.PostProcessing     (createPostProcessing,
                                         deletePostProcessing)
import           Gfx.TextRendering      (TextRenderer, addCodeTextureToLib,
                                         createTextRenderer, renderCode,
                                         renderCodebuffer,
                                         resizeTextRendererScreen)
import           Gfx.Textures           (addTexture, createTextureLib)
import           Gfx.Windowing          (setupWindow)
import           Language               (copyRNG, createGfx, updateEngineInfo,
                                         updateStateVariables)
import           Language.Ast           (Value (Number))
import           Logging                (logError, logInfo)
import           Server                 (serveComs)

import           Util                   ((/.))

main :: IO ()
main = I.createEnv >>= app

app :: ImprovizEnv -> IO ()
app env =
  let initCallback = initApp env
      resizeCallback = resize env
      displayCallback = display env
   in do serveComs env
         setupWindow env initCallback resizeCallback displayCallback

initApp :: ImprovizEnv -> Int -> Int -> IO ()
initApp env width height =
  let config = env ^. I.config
   in do post <- createPostProcessing width height
         textRenderer <- createTextRenderer config width height
         textureLib <- createTextureLib (config ^. C.textureDirectories)
         let tLibWithCode = addCodeTextureToLib textRenderer textureLib
         gfxEngineState <-
           createGfxEngineState
             config
             width
             height
             post
             textRenderer
             tLibWithCode
         atomically $ do
           putTMVar (env ^. I.graphics) gfxEngineState
           modifyTVar
             (env ^. I.language)
             (IL.updateInterpreterState (updateEngineInfo gfxEngineState))

resize :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
resize env newWidth newHeight fbWidth fbHeight =
  let config = env ^. I.config
   in do logInfo "Resizing"
         engineState <- atomically $ readTMVar (env ^. I.graphics)
         deletePostProcessing $ postFX engineState
         newPost <- createPostProcessing fbWidth fbHeight
         newTrender <-
           resizeTextRendererScreen
             config
             fbWidth
             fbHeight
             (textRenderer engineState)
         atomically $ do
           es <- takeTMVar (env ^. I.graphics)
           putTMVar
             (env ^. I.graphics)
             (resizeGfxEngine config newWidth newHeight newPost newTrender es)

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as <- readTVarIO (env ^. I.language)
  vars <- readTVarIO (env ^. I.externalVars)
  let newVars = ("time", Number $ double2Float time) : M.toList vars
      interpreterState =
        updateStateVariables newVars (as ^. IL.initialInterpreter)
      ((result, logs), nextState) =
        createGfx interpreterState (as ^. IL.currentAst)
  case result of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar (env ^. I.language) IL.resetProgram
    Right scene -> do
      gs <- atomically $ readTMVar (env ^. I.graphics)
      ui <- readTVarIO $ env ^. I.ui
      when (IL.programHasChanged as) $ do
        logInfo "Saving current ast"
        renderCode 0 0 (textRenderer gs) (ui ^. IUI.currentText)
        atomically $ modifyTVar (env ^. I.language) IL.saveProgram
      renderGfx gs scene
      when (ui ^. IUI.displayText) $ do
        renderCode 0 0 (textRenderer gs) (ui ^. IUI.currentText)
        renderCodebuffer (textRenderer gs)
  atomically $
    modifyTVar
      (env ^. I.language)
      (IL.updateInterpreterState (copyRNG nextState))
