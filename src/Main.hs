module Main where

import           GHC.Float                      ( double2Float )

import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar
                                                , readTVarIO
                                                , writeTVar
                                                )
import           Control.Monad                  ( unless
                                                , when
                                                )
import qualified Data.List                     as L
import qualified Data.Map                      as M
import           Data.Either                    ( partitionEithers )
import           Data.Maybe                     ( catMaybes )

import           Lens.Simple                    ( set
                                                , view
                                                , at
                                                , (^.)
                                                )

import           Improviz                       ( ImprovizEnv
                                                , createEnv
                                                )
import qualified Improviz                      as I
import qualified Improviz.Runtime              as IR
import qualified Improviz.UI                   as IUI
import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C

import           Gfx                            ( createGfx
                                                , renderGfx
                                                , resizeGfx
                                                )
import           Gfx.Textures                   ( createTextureLib )
import qualified Gfx.Materials                 as GM
import qualified Gfx.Engine                    as GE
import           Gfx.Context                    ( reset
                                                , renderCode
                                                , renderCodeToBuffer
                                                )

import           Windowing                      ( setupWindow )
import           Language                       ( interpret
                                                , setInterpreterVariables
                                                )
import           Language.Ast                   ( Value(Number) )
import           Language.Interpreter.Types     ( textureInfo )
import           Logging                        ( logError
                                                , logInfo
                                                )
import           Server                         ( serveComs )

main :: IO ()
main = C.getConfig >>= app

app :: ImprovizConfig -> IO ()
app config = setupWindow config (initApp config) resize display

initApp :: ImprovizConfig -> Int -> Int -> Int -> Int -> IO ImprovizEnv
initApp config width height fbWidth fbHeight = do
  logInfo $ "Running at " ++ show width ++ " by " ++ show height
  logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
  textureLib <- createTextureLib (config ^. C.textureDirectories)
  gfx        <- createGfx config textureLib width height fbWidth fbHeight
  env        <- createEnv config gfx
  serveComs env
  let ti = M.map M.size textureLib
  atomically $ modifyTVar (env ^. I.runtime)
                          (set (IR.initialInterpreter . textureInfo) ti)
  return env

resize :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
resize env newWidth newHeight fbWidth fbHeight =
  let config     = env ^. I.config
      gfxVar     = env ^. I.graphics
      runtimeVar = env ^. I.runtime
  in  do
        logInfo $ "Resizing to " ++ show newWidth ++ " by " ++ show newHeight
        logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
        runtime     <- readTVarIO runtimeVar
        engineState <- readTVarIO gfxVar
        newGfx      <- resizeGfx engineState
                                 config
                                 newWidth
                                 newHeight
                                 fbWidth
                                 fbHeight
        let newRuntime = IR.resizeRuntime newGfx runtime
        atomically $ writeTVar gfxVar newGfx
        atomically $ writeTVar runtimeVar newRuntime
        return ()

loadQueuedMaterials :: ImprovizEnv -> IO ()
loadQueuedMaterials env = do
  as <- readTVarIO (env ^. I.runtime)
  let newMaterialsData = as ^. IR.materialsToLoad
  unless
    (L.null newMaterialsData)
    (do
      (errs, newMaterials) <-
        partitionEithers <$> mapM GM.loadMaterial newMaterialsData
      mapM_ logError errs
      logInfo $ "loading " ++ show (L.length newMaterials) ++ " new materials"
      ge <- readTVarIO (env ^. I.graphics)
      let existingMaterials =
            catMaybes
              $   (\m -> view (GE.materialLibrary . at (GM.name m)) ge)
              <$> newMaterials
      let newGfx = foldl
            (\fge m -> set (GE.materialLibrary . at (GM.name m)) (Just m) fge)
            ge
            newMaterials
      atomically $ do
        writeTVar (env ^. I.runtime)  (set IR.materialsToLoad [] as)
        writeTVar (env ^. I.graphics) newGfx
      mapM_ GM.destroyMaterial existingMaterials
    )

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  loadQueuedMaterials env
  as      <- readTVarIO (env ^. I.runtime)
  extVars <- readTVarIO (env ^. I.externalVars)
  let gfxCtx     = env ^. I.gfxContext
  let globalVars = [("time", Number $ double2Float (time * 1000))] -- set time to be milliseconds
  is <- setInterpreterVariables globalVars extVars (as ^. IR.initialInterpreter)
  ui          <- readTVarIO $ env ^. I.ui
  gs          <- readTVarIO (env ^. I.graphics)
  (result, _) <- renderGfx (interpret is (as ^. IR.currentAst)) gs
  case result of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar (env ^. I.runtime) IR.resetProgram
    Right _ -> do
      reset gfxCtx
      when (IR.programHasChanged as) $ do
        logInfo "Saving current ast"
        renderCode gfxCtx (ui ^. IUI.currentText)
        atomically $ modifyTVar (env ^. I.runtime) IR.saveProgram
      when (ui ^. IUI.displayText)
        $ renderCodeToBuffer gfxCtx (ui ^. IUI.currentText)
