{-# LANGUAGE OverloadedStrings #-}

module Configuration.YamlConfig where

import           Data.Yaml                 (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml                 as Y

import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

data ImpYAMLFontConfig = ImpYAMLFontConfig
  { yamlFontFilePath :: Maybe FilePath
  , yamlFontSize     :: Maybe Int
  , yamlFontFGColour :: Maybe (Color4 GLfloat)
  , yamlFontBGColour :: Maybe (Color4 GLfloat)
  } deriving (Show)

instance (Fractional a, FromJSON a) => FromJSON (Color4 a) where
  parseJSON v = do
    (r, g, b, a) <- parseJSON v
    return $ Color4 (r / 255) (g / 255) (b / 255) (a / 255)

instance FromJSON ImpYAMLFontConfig where
  parseJSON (Y.Object v) =
    ImpYAMLFontConfig <$> v .:? "filepath" <*> v .:? "size" <*>
    v .:? "foregroundColour" <*>
    v .:? "backgroundColour"
  parseJSON _ = fail "Expected Object for Config value"

data ImpYAMLConfig = ImpYAMLConfig
  { yamlScreenWidth        :: Maybe Int
  , yamlScreenHeight       :: Maybe Int
  , yamlFullscreenDisplay  :: Maybe Int
  , yamlDebug              :: Bool
  , yamlFontCfg            :: ImpYAMLFontConfig
  , yamlTextureDirectories :: [FilePath]
  }

instance FromJSON ImpYAMLConfig where
  parseJSON (Y.Object v) =
    ImpYAMLConfig <$> v .:? "screenwidth" <*> v .:? "screenheight" <*>
    v .:? "fullscreen" <*>
    v .:? "debug" .!= False <*>
    v .:? "font" .!= ImpYAMLFontConfig Nothing Nothing Nothing Nothing <*>
    v .:? "textureDirectories" .!= []
  parseJSON _ = fail "Expected Object for Config value"

readConfigFile :: FilePath -> IO (Maybe ImpYAMLConfig)
readConfigFile cfgFilePath = do
  yaml <- Y.decodeFileEither cfgFilePath
  case yaml of
    Left err         -> print err >> return Nothing
    Right yamlConfig -> return (Just yamlConfig)
