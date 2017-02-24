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
      Just newAst -> do
        modifyMVar_ appState (\as -> return as { validAst = newAst })
        return $ mconcat ["Parsed (", newProgram, ") into ", show newAst]
      Nothing -> return "Could not parse input"
      

runServer :: MVar AppState -> IO ()
runServer appState = scotty 3000 $ do
  get "/" $ do
    text "SERVING"
  post "/read" $ do
    b <- body
    resp <- liftIO $ updateProgram appState (unpack b)
    text $ pack resp


