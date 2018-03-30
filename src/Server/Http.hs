{-# LANGUAGE OverloadedStrings #-}

module Server.Http
  ( createServer
  ) where

import           Web.Scotty

import           Control.Concurrent         (ThreadId, forkIO)
import           Control.Concurrent.STM     (TVar, atomically, modifyTVar,
                                             readTVarIO)
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Data.Aeson                 hiding (json)
import           Data.Monoid                ((<>))

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Text.Lazy             (pack)
import           Logging                    (logError, logInfo)

import           AppState                   (AppState)
import qualified AppState                   as AS
import qualified Language                   as L

import           Server.Protocol

import qualified Configuration              as C
import           Improviz                   (ImprovizApp, ImprovizEnv)
import qualified Improviz                   as I

import           Lens.Simple                ((^.))

updateProgram :: TVar AppState -> String -> IO (ImprovizResponse String)
updateProgram appState newProgram =
  case L.parse newProgram of
    Right newAst -> do
      atomically $
        modifyTVar
          appState
          (AS.clearErrors . AS.updateProgram newProgram newAst)
      let msg = "Parsed Successfully"
      logInfo msg
      return $ ImprovizOKResponse msg
    Left err -> do
      logError err
      return $ ImprovizErrorResponse err

toggleTextDisplay :: TVar AppState -> IO (ImprovizResponse String)
toggleTextDisplay appState = do
  atomically $ modifyTVar appState AS.toggleText
  let msg = "Text display toggled"
  logInfo msg
  return $ ImprovizOKResponse msg

nudgeTime :: TVar AppState -> Float -> IO (ImprovizResponse String)
nudgeTime appState nudgeAmount = do
  atomically $ modifyTVar appState (AS.nudgeBeat nudgeAmount)
  let msg = "Nudged by " <> show nudgeAmount
  logInfo msg
  return $ ImprovizOKResponse msg

getErrors :: TVar AppState -> IO (ImprovizResponse [AS.ImprovizError])
getErrors appState = do
  as <- readTVarIO appState
  let errs = AS.getErrors as
  logInfo $ "Have " <> show (length errs) <> " Errors"
  return $ ImprovizErrorResponse errs

createServer :: (MonadIO m) => ImprovizApp (m ThreadId)
createServer = do
  env <- ask
  return $ liftIO $ forkIO $ scotty (env ^. I.config . C.serverPort) $ do
    get "/" $ text "SERVING"
    post "/read" $ do
      b <- body
      resp <- liftIO $ updateProgram (env ^. I.appstate) (unpack b)
      json resp
    post "/toggle/text" $ do
      resp <- liftIO $ toggleTextDisplay (env ^. I.appstate)
      json resp
    post "/nudge/:amount" $ do
      amount <- param "amount"
      resp <- liftIO $ nudgeTime (env ^. I.appstate) amount
      json resp
    post "/errors" $ do
      resp <- liftIO $ getErrors (env ^. I.appstate)
      json resp
