module Main where

import           GHC.Float              (double2Float)

import           Control.Concurrent.STM (atomically, modifyTVar, readTMVar,
                                         readTVarIO, swapTVar, takeTMVar)
import           Control.Monad          (when)
import qualified Data.Map.Strict        as M

import           Lens.Simple            ((^.))

import qualified Configuration          as C
import           Improviz               (ImprovizEnv, ImprovizError (..))
import qualified Improviz               as I
import qualified Improviz.Language      as IL
import qualified Improviz.UI            as IUI

import           Gfx                    (EngineState (..), createGfxEngine,
                                         renderGfx, resizeGfx)
import           Gfx.TextRendering      (renderCode, renderCodebuffer)
import           Gfx.Windowing          (setupWindow)
import           Language               (createGfx, updateEngineInfo,
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
      gfxVar = env ^. I.graphics
   in do gfx <- createGfxEngine config width height
         atomically $ swapTVar gfxVar gfx
         return ()

resize :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
resize env newWidth newHeight fbWidth fbHeight =
  let config = env ^. I.config
      gfxVar = env ^. I.graphics
   in do logInfo $ "Resizing to " ++ show newWidth ++ " by " ++ show newHeight
         engineState <- readTVarIO gfxVar
         newGfx <-
           resizeGfx engineState config newWidth newHeight fbWidth fbHeight
         atomically $ swapTVar gfxVar newGfx
         return ()

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as <- readTVarIO (env ^. I.language)
  vars <- readTVarIO (env ^. I.externalVars)
  let newVars = ("time", Number $ double2Float time) : M.toList vars
      is = updateStateVariables newVars (as ^. IL.initialInterpreter)
      ((result, _), _) = createGfx is (as ^. IL.currentAst)
  case result of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar (env ^. I.language) IL.resetProgram
    Right scene -> do
      gs <- readTVarIO (env ^. I.graphics)
      ui <- readTVarIO $ env ^. I.ui
      when (IL.programHasChanged as) $ do
        logInfo "Saving current ast"
        renderCode 0 0 (textRenderer gs) (ui ^. IUI.currentText)
        atomically $ modifyTVar (env ^. I.language) IL.saveProgram
      renderGfx gs scene
      when (ui ^. IUI.displayText) $ do
        renderCode 0 0 (textRenderer gs) (ui ^. IUI.currentText)
        renderCodebuffer (textRenderer gs)
