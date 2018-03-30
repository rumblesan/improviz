{-# LANGUAGE TemplateHaskell #-}

module Improviz where

import           Control.Concurrent.STM (TMVar, TVar, newEmptyTMVarIO,
                                         newTVarIO)
import           Control.Monad.Reader

import           Lens.Simple

import           AppState               (AppState, makeAppState)
import           Configuration          (ImprovizConfig, getConfig)
import           Gfx.EngineState        (EngineState)

data ImprovizEnv = ImprovizEnv
  { _starttime :: TVar Float
  , _appstate  :: TVar AppState
  , _gfxstate  :: TMVar EngineState
  , _config    :: ImprovizConfig
  }

makeLenses ''ImprovizEnv

createEnv :: IO ImprovizEnv
createEnv =
  ImprovizEnv <$> newTVarIO 0.0 <*> newTVarIO (makeAppState 0.0) <*>
  newEmptyTMVarIO <*>
  getConfig
