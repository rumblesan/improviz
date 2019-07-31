{-# LANGUAGE OverloadedStrings #-}

module Server.Protocol where

import           Data.Aeson

import           Language.Parser.Errors         ( ImprovizCodeError(..) )

data ImprovizResponse
  = ImprovizOKResponse String
  | ImprovizErrorResponse String
  | ImprovizCodeErrorResponse [ImprovizCodeError]
  deriving (Show, Eq)

instance ToJSON ImprovizResponse where
  toJSON (ImprovizOKResponse payload) =
    object [("status", "ok"), "payload" .= payload]
  toJSON (ImprovizErrorResponse payload) =
    object [("status", "server-error"), "payload" .= payload]
  toJSON (ImprovizCodeErrorResponse payload) =
    object [("status", "error"), "payload" .= payload]
