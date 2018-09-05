{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Improviz where

import           GHC.Generics

import           Control.Concurrent.STM (TMVar, TVar, newEmptyTMVarIO,
                                         newTVarIO)
import           Data.Time.Clock.POSIX  (POSIXTime, getPOSIXTime)

import           Lens.Simple

import           Data.Aeson

import           Configuration          (ImprovizConfig, getConfig)
import           Gfx.EngineState        (EngineState)

import           Improviz.Language      (ImprovizLanguage, makeLanguageState)
import           Improviz.UI            (ImprovizUI, defaultUI)

data ImprovizError = ImprovizError
  { text     :: String
  , position :: Maybe (Int, Int)
  } deriving (Eq, Show, Generic)

instance ToJSON ImprovizError where
  toEncoding = genericToEncoding defaultOptions

data ImprovizEnv = ImprovizEnv
  { _language  :: TVar ImprovizLanguage
  , _ui        :: TVar ImprovizUI
  , _graphics  :: TMVar EngineState
  , _config    :: ImprovizConfig
  , _startTime :: POSIXTime
  , _errors    :: TVar [ImprovizError]
  }

makeLenses ''ImprovizEnv

createEnv :: IO ImprovizEnv
createEnv = do
  startTime <- getPOSIXTime
  languageState <- newTVarIO $ makeLanguageState (round $ realToFrac startTime)
  uiState <- newTVarIO defaultUI
  gfxState <- newEmptyTMVarIO
  config <- getConfig
  errors <- newTVarIO []
  return $ ImprovizEnv languageState uiState gfxState config startTime errors
