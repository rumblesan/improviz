{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration.OSC where

import           Lens.Simple (makeLenses, (^.))

import           Data.Yaml   (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml   as Y

data ImprovizOSCConfig = ImprovizOSCConfig
  { _enabled :: Bool
  , _port    :: Int
  } deriving (Show)

makeLenses ''ImprovizOSCConfig

defaultOSCConfig = ImprovizOSCConfig {_enabled = False, _port = 5510}

instance FromJSON ImprovizOSCConfig where
  parseJSON (Y.Object v) =
    ImprovizOSCConfig <$> v .:? "enabled" .!= (defaultOSCConfig ^. enabled) <*>
    v .:? "port" .!= (defaultOSCConfig ^. port)
  parseJSON _ = fail "Expected Object for OSCConfig value"
