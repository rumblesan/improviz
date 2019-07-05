module Main where

import           GHC.Float                      ( double2Float )

import           Control.Concurrent.STM         ( atomically
                                                , modifyTVar
                                                , readTVarIO
                                                , takeTMVar
                                                , putTMVar
                                                )
import           Control.Monad                  ( when )
import qualified Data.Map.Strict               as M

import           Lens.Simple                    ( set
                                                , (^.)
                                                )

import           Improviz                       ( ImprovizEnv )
import qualified Improviz                      as I
import qualified Improviz.Language             as IL
import qualified Improviz.UI                   as IUI
import qualified Configuration                 as C

import           Gfx                            ( createGfx
                                                , renderGfx
                                                , resizeGfx
                                                , updateGfx
                                                , renderCode
                                                , renderCodeToBuffer
                                                )
import           Gfx.Textures                   ( TextureInfo(..)
                                                , createTextureLib
                                                )
import           Windowing                      ( setupWindow )
import           Language                       ( interpret
                                                , updateStateVariables
                                                , setGfxEngine
                                                , getGfxEngine
                                                )
import           Language.Ast                   ( Value(Number) )
import           Language.Interpreter.Types     ( textureInfo )
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
  let config   = env ^. I.config
      gfxVar   = env ^. I.graphics
      language = env ^. I.language
  in  do
        logInfo $ "Running at " ++ show width ++ " by " ++ show height
        logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
        textureLib <- createTextureLib (config ^. C.textureDirectories)
        gfx        <- createGfx config textureLib width height fbWidth fbHeight
        let ti = TextureInfo $ M.map M.size textureLib
        atomically $ do
          putTMVar gfxVar gfx
          modifyTVar language (set (IL.initialInterpreter . textureInfo) ti)
        return ()

resize :: ImprovizEnv -> Int -> Int -> Int -> Int -> IO ()
resize env newWidth newHeight fbWidth fbHeight =
  let config = env ^. I.config
      gfxVar = env ^. I.graphics
  in  do
        logInfo $ "Resizing to " ++ show newWidth ++ " by " ++ show newHeight
        logInfo $ "Framebuffer " ++ show fbWidth ++ " by " ++ show fbHeight
        engineState <- atomically $ takeTMVar gfxVar
        newGfx      <- resizeGfx engineState
                                 config
                                 newWidth
                                 newHeight
                                 fbWidth
                                 fbHeight
        atomically $ putTMVar gfxVar newGfx
        return ()

initialVars :: M.Map String Value -> Float -> [(String, Value)]
initialVars vars t =
  ("time", Number t) : ("nudge", Number 0) : ("bpm", Number 120) : M.toList vars

display :: ImprovizEnv -> Double -> IO ()
display env time = do
  as   <- readTVarIO (env ^. I.language)
  vars <- readTVarIO (env ^. I.externalVars)
  gs   <- atomically $ takeTMVar (env ^. I.graphics)
  let newVars = initialVars vars (double2Float time)
  is <- setGfxEngine gs
    <$> updateStateVariables newVars (as ^. IL.initialInterpreter)


  ui               <- readTVarIO $ env ^. I.ui
  (result, postIS) <- renderGfx (interpret is (as ^. IL.currentAst)) gs

  case (result, getGfxEngine postIS) of
    (Left msg, _) -> do
      logError $ "Could not interpret program: " ++ msg
      atomically $ modifyTVar (env ^. I.language) IL.resetProgram
    (_, Nothing    ) -> logError "No Graphics Engine"
    (_, Just gfxEng) -> do
      atomically $ putTMVar (env ^. I.graphics) (updateGfx gs gfxEng)
      when (IL.programHasChanged as) $ do
        logInfo "Saving current ast"
        renderCode gs (ui ^. IUI.currentText)
        atomically $ modifyTVar (env ^. I.language) IL.saveProgram
      when (ui ^. IUI.displayText)
        $ renderCodeToBuffer gs (ui ^. IUI.currentText)
