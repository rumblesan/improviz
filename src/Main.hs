module Main where

import           GHC.Float                      ( double2Float )

import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar
                                                , readTVarIO
                                                , swapTVar
                                                )
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as M

import           Lens.Simple                    ( (^.) )

import           Improviz                       ( ImprovizEnv )
import qualified Improviz                      as I
import qualified Improviz.Language             as IL
import qualified Improviz.UI                   as IUI

import           Gfx                            ( EngineState(textRenderer)
                                                , createGfx
                                                , renderGfx
                                                , resizeGfx
                                                )
import           Gfx.TextRendering              ( renderCode
                                                , renderCodebuffer
                                                )
import           Gfx.Windowing                  ( setupWindow )
import           Language                       ( createGfxScene
                                                , updateStateVariables
                                                )
import           Language.Ast                   ( Value(Number) )
import           Logging                        ( logError
                                                , logInfo
                                                )
import           Server                         ( serveComs )

main :: IO ()
main = I.createEnv >>= app

app :: ImprovizEnv -> IO ()
app env =
  let initCallback    = initApp env
      resizeCallback  = resize env
      displayCallback = display env
  in  do
        serveComs env
        setupWindow env initCallback resizeCallback displayCallback

initApp :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
initApp env width height fbWidth fbHeight =
  let config = env ^. I.config
      gfxVar = env ^. I.graphics
  in  do
        logInfo $ "Running at " ++ show width ++ " by " ++ show height
        logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
        gfx <- createGfx config width height fbWidth fbHeight
        atomically $ swapTVar gfxVar gfx
        return ()

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
        atomically $ swapTVar gfxVar newGfx
        return ()

initialVars :: M.Map String Value -> Float -> [(String, Value)]
initialVars vars t =
  ("time", Number t) : ("nudge", Number 0) : ("bpm", Number 120) : M.toList vars

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as   <- readTVarIO (env ^. I.language)
  vars <- readTVarIO (env ^. I.externalVars)
  let newVars = initialVars vars (double2Float time)
      is      = updateStateVariables newVars (as ^. IL.initialInterpreter)
      result  = fst $ createGfxScene is (as ^. IL.currentAst)
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
