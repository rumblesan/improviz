{-# LANGUAGE OverloadedStrings #-}

module Server.Protocol where

import           Data.Aeson
import           Data.String (fromString)

data (ToJSON e) =>
     ImprovizResponse e
  = ImprovizOKResponse String
  | ImprovizErrorResponse e
  deriving (Show, Eq)

instance (ToJSON e) => ToJSON (ImprovizResponse e) where
  toJSON (ImprovizOKResponse payload) =
    object [("status", "ok"), "payload" .= payload]
  toJSON (ImprovizErrorResponse payload) =
    object [("status", "error"), "payload" .= payload]
