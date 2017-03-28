{-# LANGUAGE OverloadedStrings #-}

module AppServer where

import Web.Scotty

import Data.Monoid (mconcat)
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import Data.ByteString.Lazy.Char8 (unpack)
import Data.Text.Lazy (pack)

import AppTypes
import qualified Language as L

updateProgram :: MVar AppState -> String -> IO String
updateProgram appState newProgram =
  let
    ast = L.parse newProgram
  in
    case ast of
      Right newAst -> do
        modifyMVar_ appState (\as -> return as { currentAst = newAst })
        return "{'status': 'OK', 'message': 'Parsed Succesfully'}"
      Left err -> return $ mconcat ["{'status': 'OK', 'message': '", err, "'}"]

runServer :: MVar AppState -> IO ()
runServer appState = scotty 3000 $ do
  get "/" $ text "SERVING"
  post "/read" $ do
    b <- body
    resp <- liftIO $ updateProgram appState (unpack b)
    liftIO $ print resp
    json $ pack resp


