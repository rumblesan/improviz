{-# LANGUAGE OverloadedStrings #-}

module Server.Http
  ( startHttpServer
  ) where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Static
import           Web.Scotty

import           Control.Concurrent             ( ThreadId
                                                , forkIO
                                                )
import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar
                                                )
import           Control.Monad.Trans            ( liftIO )

import           Data.ByteString.Lazy.Char8     ( ByteString
                                                , unpack
                                                )
import qualified Data.Map.Strict               as M
import           Logging                        ( logError
                                                , logInfo
                                                )

import qualified Configuration.Shaders         as CS
import qualified Language                      as L
import           Language.Ast                   ( Value(Number) )
import           Language.Parser.Errors         ( parseErrorsOut
                                                , prettyPrintErrors
                                                )
import           Server.Protocol

import qualified Configuration                 as C
import           Improviz                       ( ImprovizEnv )
import qualified Improviz                      as I
import qualified Improviz.Runtime              as IR
import           Improviz.UI                    ( ImprovizUI )
import qualified Improviz.UI                   as IUI

import           Lens.Simple                    ( (^.)
                                                , set
                                                )

updateProgram :: ImprovizEnv -> String -> IO ImprovizResponse
updateProgram env newProgram = case L.parse newProgram of
  Right newAst -> do
    atomically $ do
      modifyTVar (env ^. I.runtime) (IR.updateProgram newProgram newAst)
      modifyTVar (env ^. I.ui)      (set IUI.currentText newProgram)
    let msg = "Parsed Successfully"
    logInfo msg
    return $ ImprovizOKResponse msg
  Left err -> do
    logError $ prettyPrintErrors err
    return $ ImprovizCodeErrorResponse $ parseErrorsOut err

updateMaterial :: ImprovizEnv -> ByteString -> IO ImprovizResponse
updateMaterial env newMaterial = case CS.loadShaderString newMaterial of
  Right materialData -> do
    atomically $ modifyTVar (env ^. I.runtime) (addToMaterialQueue materialData)
    let msg = "Material Queued Successfully"
    logInfo msg
    return $ ImprovizOKResponse msg
  Left err -> do
    logError err
    return $ ImprovizErrorResponse err
 where
  addToMaterialQueue md rt =
    set IR.materialsToLoad (md : rt ^. IR.materialsToLoad) rt

toggleTextDisplay :: TVar ImprovizUI -> IO ImprovizResponse
toggleTextDisplay ui = do
  atomically $ modifyTVar ui IUI.toggleTextDisplay
  let msg = "Text display toggled"
  logInfo msg
  return $ ImprovizOKResponse msg

updateExternalVar :: ImprovizEnv -> String -> Float -> IO ImprovizResponse
updateExternalVar env name value = do
  atomically $ modifyTVar (env ^. I.externalVars) (M.insert name (Number value))
  return $ ImprovizOKResponse $ name ++ " variable updated"


startHttpServer :: ImprovizEnv -> IO ThreadId
startHttpServer env =
  let port     = (env ^. I.config . C.serverPort)
      settings = setPort port defaultSettings
      options  = Options { verbose = 0, settings = settings }
  in  do
        logInfo $ "Improviz HTTP server listening on port " ++ show port
        forkIO $ scottyOpts options $ do
          middleware $ staticPolicy (noDots >-> addBase "assets/static")
          post "/read/material" $ do
            b    <- body
            resp <- liftIO $ updateMaterial env b
            json resp
          post "/read" $ do
            b    <- body
            resp <- liftIO $ updateProgram env (unpack b)
            json resp
          post "/toggle/text" $ do
            resp <- liftIO $ toggleTextDisplay (env ^. I.ui)
            json resp
          post "/vars/edit/:name" $ do
            name <- param "name"
            b    <- body
            case reads (unpack b) of
              [(v, _)] -> do
                resp <- liftIO $ updateExternalVar env name v
                json resp
              _ ->
                json
                  $  ImprovizErrorResponse
                  $  name
                  ++ " variable not updated. Value invalid"
          get "/" $ text "SERVING"
          get "/editor" $ redirect "/editor/index.html"
