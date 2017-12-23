{-# LANGUAGE OverloadedStrings #-}

module AppServer where

import           Web.Scotty

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid                ((<>))

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Text.Lazy             (pack)
import           Logging                    (logError, logInfo)

import           AppState                   (AppState)
import qualified AppState                   as AS
import qualified Language                   as L

updateProgram :: TVar AppState -> String -> IO String
updateProgram appState newProgram =
  case L.parse newProgram of
    Right newAst -> do
      atomically $ modifyTVar appState (AS.updateProgram newProgram newAst)
      logInfo "Parsed succesfully"
      return "{'status': 'OK', 'message': 'Parsed Succesfully'}"
    Left err -> do
      logError err
      return $ "{'status': 'OK', 'message': '" <> err <> "'}"

toggleTextDisplay :: TVar AppState -> IO String
toggleTextDisplay appState = do
  atomically $ modifyTVar appState AS.toggleText
  logInfo "Text display toggled"
  return "{'status': 'OK', 'message': 'Toggled text display'}"

nudgeTime :: TVar AppState -> Float -> IO String
nudgeTime appState nudgeAmount = do
  atomically $ modifyTVar appState (AS.nudgeBeat nudgeAmount)
  logInfo $ "Nudged by " <> show nudgeAmount
  return $ "{'status': 'OK', 'message': 'Nudged by " <> show nudgeAmount <> "'}"

runServer :: TVar AppState -> Int -> IO ()
runServer appState port =
  scotty port $ do
    get "/" $ text "SERVING"
    post "/read" $ do
      b <- body
      resp <- liftIO $ updateProgram appState (unpack b)
      json $ pack resp
    post "/toggle/text" $ do
      resp <- liftIO $ toggleTextDisplay appState
      json $ pack resp
    post "/nudge/:amount" $ do
      amount <- param "amount"
      resp <- liftIO $ nudgeTime appState amount
      json $ pack resp
