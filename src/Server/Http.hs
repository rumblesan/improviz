{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Server.Http
  ( startHttpServer
  ) where

import           Network.Wai.Handler.Warp
import           Web.Scotty

import           Data.FileEmbed             (embedFile)

import           Control.Concurrent         (ThreadId, forkIO)
import           Control.Concurrent.STM     (TVar, atomically, modifyTVar)
import           Control.Monad.Trans        (liftIO)
import           Data.Aeson                 hiding (json)
import           Data.Monoid                ((<>))

import qualified Data.ByteString.Lazy       as BL
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Text.Lazy             (pack)
import           Logging                    (logError, logInfo)

import qualified Language                   as L

import           Server.Protocol

import qualified Configuration              as C
import           Improviz                   (ImprovizEnv)
import qualified Improviz                   as I
import           Improviz.Language          (ImprovizLanguage)
import qualified Improviz.Language          as IL
import           Improviz.UI                (ImprovizUI)
import qualified Improviz.UI                as IUI

import           Lens.Simple                (set, (^.))

simpleEditorHtml :: BL.ByteString
simpleEditorHtml = BL.fromStrict $(embedFile "src/assets/html/editor.html")

updateProgram :: ImprovizEnv -> String -> IO (ImprovizResponse String)
updateProgram env newProgram =
  case L.parse newProgram of
    Right newAst -> do
      atomically $ do
        modifyTVar (env ^. I.language) (IL.updateProgram newProgram newAst)
        modifyTVar (env ^. I.ui) (set IUI.currentText newProgram)
      let msg = "Parsed Successfully"
      logInfo msg
      return $ ImprovizOKResponse msg
    Left err -> do
      logError err
      return $ ImprovizErrorResponse err

toggleTextDisplay :: TVar ImprovizUI -> IO (ImprovizResponse String)
toggleTextDisplay ui = do
  atomically $ modifyTVar ui IUI.toggleTextDisplay
  let msg = "Text display toggled"
  logInfo msg
  return $ ImprovizOKResponse msg

startHttpServer :: ImprovizEnv -> IO ThreadId
startHttpServer env =
  let port = (env ^. I.config . C.serverPort)
      settings = setPort port defaultSettings
      options = Options {verbose = 0, settings = settings}
   in do logInfo $ "Improviz HTTP server listening on port " ++ show port
         forkIO $ scottyOpts options $ do
           get "/" $ text "SERVING"
           get "/editor" $ raw simpleEditorHtml
           post "/read" $ do
             b <- body
             resp <- liftIO $ updateProgram env (unpack b)
             json resp
           post "/toggle/text" $ do
             resp <- liftIO $ toggleTextDisplay (env ^. I.ui)
             json resp
