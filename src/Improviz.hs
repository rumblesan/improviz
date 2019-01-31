{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Improviz
  ( ImprovizEnv
  , language
  , ui
  , graphics
  , config
  , startTime
  , externalVars
  , createEnv
  ) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map.Strict        as M
import           Data.Time.Clock.POSIX  (POSIXTime, getPOSIXTime)

import           Lens.Simple

import           Configuration          (ImprovizConfig, getConfig)
import           Gfx                    (emptyGfx)
import           Gfx.EngineState        (EngineState)

import           Improviz.Language      (ImprovizLanguage, makeLanguageState)
import           Improviz.UI            (ImprovizUI, defaultUI)
import qualified Language.Ast           as LA

data ImprovizEnv = ImprovizEnv
  { _language     :: TVar ImprovizLanguage
  , _ui           :: TVar ImprovizUI
  , _graphics     :: TVar EngineState
  , _config       :: ImprovizConfig
  , _startTime    :: POSIXTime
  , _externalVars :: TVar (M.Map String LA.Value)
  }

makeLenses ''ImprovizEnv

createEnv :: IO ImprovizEnv
createEnv = do
  startTime <- getPOSIXTime
  languageState <- newTVarIO makeLanguageState
  uiState <- newTVarIO defaultUI
  config <- getConfig
  gfxState <- newTVarIO emptyGfx
  externalVars <- newTVarIO M.empty
  uiState <- newTVarIO defaultUI
  return $
    ImprovizEnv languageState uiState gfxState config startTime externalVars
