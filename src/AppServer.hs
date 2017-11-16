{-# LANGUAGE OverloadedStrings #-}

module AppServer where

import           Web.Scotty

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid                (mconcat)

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Text.Lazy             (pack)

import           AppTypes
import qualified Language                   as L

updateProgram :: TVar AppState -> String -> IO String
updateProgram appState newProgram =
  let ast = L.parse newProgram
  in case ast of
       Right newAst -> do
         atomically $
           modifyTVar
             appState
             (\as -> as {currentAst = newAst, programText = newProgram})
         return "{'status': 'OK', 'message': 'Parsed Succesfully'}"
       Left err -> return $ mconcat ["{'status': 'OK', 'message': '", err, "'}"]

toggleTextDisplay :: TVar AppState -> IO String
toggleTextDisplay appState = do
  atomically $
    modifyTVar appState (\as -> as {displayText = not $ displayText as})
  return "{'status': 'OK', 'message': 'Toggled text display'}"

runServer :: TVar AppState -> IO ()
runServer appState =
  scotty 3000 $ do
    get "/" $ text "SERVING"
    post "/read" $ do
      b <- body
      resp <- liftIO $ updateProgram appState (unpack b)
      liftIO $ print resp
      json $ pack resp
    post "/toggle/text" $ do
      resp <- liftIO $ toggleTextDisplay appState
      liftIO $ print resp
      json $ pack resp
