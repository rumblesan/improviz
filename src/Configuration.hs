{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration
  ( ImprovizConfig
  , screenWidth
  , screenHeight
  , fullscreenDisplay
  , debug
  , fontConfig
  , textureDirectories
  , serverPort
  , getConfig
  ) where

import           Control.Applicative ((<|>))
import           Data.Maybe          (fromMaybe)
import           Options.Applicative (execParser)

import           Lens.Simple

import           Configuration.CLI   (ImprovizCLIConfig)
import qualified Configuration.CLI   as CLI
import           Configuration.Font  (ImprovizFontConfig, defaultFontConfig)
import           Data.Yaml           (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml           as Y

data ImprovizConfig = ImprovizConfig
  { _screenWidth        :: Int
  , _screenHeight       :: Int
  , _fullscreenDisplay  :: Maybe Int
  , _debug              :: Bool
  , _fontConfig         :: ImprovizFontConfig
  , _textureDirectories :: [FilePath]
  , _serverPort         :: Int
  } deriving (Show)

makeLenses ''ImprovizConfig

defaultConfigFile :: FilePath
defaultConfigFile = "./improviz.yaml"

defaultConfig :: ImprovizConfig
defaultConfig =
  ImprovizConfig
  { _screenWidth = 640
  , _screenHeight = 480
  , _fullscreenDisplay = Nothing
  , _debug = False
  , _fontConfig = defaultFontConfig
  , _textureDirectories = ["./textures"]
  , _serverPort = 3000
  }

instance FromJSON ImprovizConfig where
  parseJSON (Y.Object v) =
    ImprovizConfig <$> v .:? "screenwidth" .!= (defaultConfig ^. screenWidth) <*>
    v .:? "screenheight" .!= (defaultConfig ^. screenHeight) <*>
    v .:? "fullscreen" <*>
    v .:? "debug" .!= False <*>
    v .:? "font" .!= defaultFontConfig <*>
    v .:? "textureDirectories" .!= (defaultConfig ^. textureDirectories) <*>
    v .:? "serverPort" .!= (defaultConfig ^. serverPort)
  parseJSON _ = fail "Expected Object for Config value"

getConfig :: IO ImprovizConfig
getConfig = do
  cliCfg <- execParser CLI.opts
  fileCfg <-
    readConfigFile $ fromMaybe defaultConfigFile (cliCfg ^. CLI.configFilePath)
  let cfg = fromMaybe defaultConfig fileCfg
  return $ mergeConfigs cfg cliCfg

readConfigFile :: FilePath -> IO (Maybe ImprovizConfig)
readConfigFile cfgFilePath = do
  yaml <- Y.decodeFileEither cfgFilePath
  case yaml of
    Left err     -> print err >> return Nothing
    Right config -> return (Just config)

mergeConfigs :: ImprovizConfig -> ImprovizCLIConfig -> ImprovizConfig
mergeConfigs cfg cliCfg =
  cfg
  { _screenWidth = fromMaybe (cfg ^. screenWidth) (cliCfg ^. CLI.screenWidth)
  , _screenHeight = fromMaybe (cfg ^. screenHeight) (cliCfg ^. CLI.screenHeight)
  , _fullscreenDisplay =
      (cfg ^. fullscreenDisplay) <|> (cliCfg ^. CLI.fullscreenDisplay)
  , _debug = (cfg ^. debug) || (cliCfg ^. CLI.debug)
  }
