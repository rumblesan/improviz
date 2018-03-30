{-# LANGUAGE TemplateHaskell #-}

module Improviz where

import           Control.Concurrent.STM (TMVar, TVar, newEmptyTMVarIO,
                                         newTVarIO)
import           Control.Monad.Reader   (Reader (..))

import           Lens.Simple

import           AppState               (AppState, makeAppState)
import           Configuration          (ImprovizConfig, getConfig)
import           Gfx.EngineState        (EngineState)

type ImprovizApp = Reader ImprovizEnv

data ImprovizEnv = ImprovizEnv
  { _appstate :: TVar AppState
  , _gfxstate :: TMVar EngineState
  , _config   :: ImprovizConfig
  }

makeLenses ''ImprovizEnv

createEnv :: Float -> IO ImprovizEnv
createEnv startTime =
  ImprovizEnv <$> newTVarIO (makeAppState startTime) <*> newEmptyTMVarIO <*>
  getConfig
