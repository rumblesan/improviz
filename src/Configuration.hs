{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Configuration where

import           Control.Applicative  ((<|>))
import           Data.Maybe           (fromMaybe)
import           Options.Applicative  (execParser)

import           Lens.Simple

import           Configuration.CLI    (ImprovizCLIConfig)
import qualified Configuration.CLI    as CLI
import           Configuration.Font   (ImprovizFontConfig, defaultFontConfig)
import           Configuration.Screen (ImprovizScreenConfig,
                                       defaultScreenConfig)
import           Data.Yaml            (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Yaml            as Y

data ImprovizConfig = ImprovizConfig
  { _screenWidth        :: Int
  , _screenHeight       :: Int
  , _fullscreenDisplay  :: Maybe Int
  , _debug              :: Bool
  , _screen             :: ImprovizScreenConfig
  , _fontConfig         :: ImprovizFontConfig
  , _textureDirectories :: [FilePath]
  , _serverPort         :: Int
  , _apptitle           :: String
  , _decorated          :: Bool
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
    , _screen = defaultScreenConfig
    , _fontConfig = defaultFontConfig
    , _textureDirectories = ["./textures"]
    , _serverPort = 3000
    , _apptitle = "Improviz"
    , _decorated = False
    }

instance FromJSON ImprovizConfig where
  parseJSON (Y.Object v) =
    ImprovizConfig <$> v .:? "screenwidth" .!= (defaultConfig ^. screenWidth) <*>
    v .:? "screenheight" .!= (defaultConfig ^. screenHeight) <*>
    v .:? "fullscreen" <*>
    v .:? "debug" .!= (defaultConfig ^. debug) <*>
    v .:? "screen" .!= (defaultConfig ^. screen) <*>
    v .:? "font" .!= (defaultConfig ^. fontConfig) <*>
    v .:? "textureDirectories" .!= (defaultConfig ^. textureDirectories) <*>
    v .:? "serverPort" .!= (defaultConfig ^. serverPort) <*>
    v .:? "apptitle" .!= (defaultConfig ^. apptitle) <*>
    v .:? "decorated" .!= (defaultConfig ^. decorated)
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
    , _screenHeight =
        fromMaybe (cfg ^. screenHeight) (cliCfg ^. CLI.screenHeight)
    , _fullscreenDisplay =
        (cfg ^. fullscreenDisplay) <|> (cliCfg ^. CLI.fullscreenDisplay)
    , _debug = (cfg ^. debug) || (cliCfg ^. CLI.debug)
    }
