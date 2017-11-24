{-# LANGUAGE OverloadedStrings #-}

module Configuration.YamlConfig where

import           Control.Applicative

import           Data.Yaml           (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml           as Y

data ImpYAMLConfig = ImpYAMLConfig
  { yamlScreenWidth        :: Maybe Int
  , yamlScreenHeight       :: Maybe Int
  , yamlFullscreenDisplay  :: Maybe Int
  , yamlDebug              :: Bool
  , yamlFontFilePath       :: Maybe FilePath
  , yamlFontSize           :: Maybe Int
  , yamlTextureDirectories :: [FilePath]
  }

instance FromJSON ImpYAMLConfig where
  parseJSON (Y.Object v) =
    ImpYAMLConfig <$> v .:? "screenwidth" <*> v .:? "screenheight" <*>
    v .:? "fullscreen" <*>
    v .:? "debug" .!= False <*>
    v .:? "fontfile" <*>
    v .:? "fontsize" <*>
    v .:? "textureDirectories" .!= []
  parseJSON _ = fail "Expected Object for Config value"

readConfigFile :: FilePath -> IO (Maybe ImpYAMLConfig)
readConfigFile cfgFilePath = do
  yaml <- Y.decodeFileEither cfgFilePath
  case yaml of
    Left err         -> print err >> return Nothing
    Right yamlConfig -> return (Just yamlConfig)
