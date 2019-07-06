module Main where

import           GHC.Float                      ( double2Float )

import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar
                                                , readTVarIO
                                                , writeTVar
                                                )
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as M

import           Lens.Simple                    ( set
                                                , (^.)
                                                )

import           Improviz                       ( ImprovizEnv
                                                , createEnv
                                                )
import qualified Improviz                      as I
import qualified Improviz.Language             as IL
import qualified Improviz.UI                   as IUI
import           Configuration                  ( ImprovizConfig )
import qualified Configuration                 as C

import           Gfx                            ( createGfx
                                                , renderGfx
                                                , resizeGfx
                                                , renderCode
                                                , renderCodeToBuffer
                                                )
import           Gfx.Textures                   ( TextureInfo(..)
                                                , createTextureLib
                                                )
import           Gfx.Context                    ( reset )

import           Windowing                      ( setupWindow )
import           Language                       ( interpret
                                                , updateStateVariables
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
  let ti = TextureInfo $ M.map M.size textureLib
  atomically $ modifyTVar (env ^. I.language)
                          (set (IL.initialInterpreter . textureInfo) ti)
  return env

resize :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
resize env newWidth newHeight fbWidth fbHeight =
  let config = env ^. I.config
      gfxVar = env ^. I.graphics
  in  do
        logInfo $ "Resizing to " ++ show newWidth ++ " by " ++ show newHeight
        logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
        engineState <- readTVarIO gfxVar
        newGfx      <- resizeGfx engineState
                                 config
                                 newWidth
                                 newHeight
                                 fbWidth
                                 fbHeight
        atomically $ writeTVar gfxVar newGfx
        return ()

initialVars :: M.Map String Value -> Float -> [(String, Value)]
initialVars vars t =
  ("time", Number t) : ("nudge", Number 0) : ("bpm", Number 120) : M.toList vars

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as   <- readTVarIO (env ^. I.language)
  vars <- readTVarIO (env ^. I.externalVars)
  gs   <- readTVarIO (env ^. I.graphics)
  let gfxCtx  = env ^. I.gfxContext
  let newVars = initialVars vars (double2Float time)
  is               <- updateStateVariables newVars (as ^. IL.initialInterpreter)

  ui               <- readTVarIO $ env ^. I.ui
  (result, postIS) <- renderGfx (interpret is (as ^. IL.currentAst)) gs

  case result of
    Left msg -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar (env ^. I.language) IL.resetProgram
    Right _ -> do
      reset gfxCtx
      when (IL.programHasChanged as) $ do
        logInfo "Saving current ast"
        renderCode gs (ui ^. IUI.currentText)
        atomically $ modifyTVar (env ^. I.language) IL.saveProgram
      when (ui ^. IUI.displayText)
        $ renderCodeToBuffer gs (ui ^. IUI.currentText)
