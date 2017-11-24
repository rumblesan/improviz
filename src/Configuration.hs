module Configuration
  ( ImpConfig(..)
  , getConfig
  ) where

import           Control.Applicative
import qualified Data.ByteString          as B
import           Data.Maybe               (fromMaybe)
import           Options.Applicative      (execParser)

import           Configuration.CLIConfig  (ImpCLIConfig (..), cliopts,
                                           cliparser)
import           Configuration.YamlConfig (ImpYAMLConfig (..), readConfigFile)

data ImpConfig = ImpConfig
  { screenWidth        :: Int
  , screenHeight       :: Int
  , fullscreenDisplay  :: Maybe Int
  , debug              :: Bool
  , fontFilePath       :: Maybe FilePath
  , fontSize           :: Int
  , textureDirectories :: [FilePath]
  } deriving (Show)

defaultConfigFile :: FilePath
defaultConfigFile = "./improviz.yaml"

defaultConfig :: ImpConfig
defaultConfig =
  ImpConfig
  { screenWidth = 640
  , screenHeight = 480
  , fullscreenDisplay = Nothing
  , debug = False
  , fontFilePath = Nothing
  , fontSize = 36
  , textureDirectories = []
  }

getConfig :: IO ImpConfig
getConfig = do
  cliCfg <- execParser cliopts
  yamlCfgOpt <-
    readConfigFile $ fromMaybe defaultConfigFile $ cliConfigFilePath cliCfg
  return
    ImpConfig
    { screenWidth =
        fromMaybe
          (screenWidth defaultConfig)
          (cliScreenWidth cliCfg <|> (yamlCfgOpt >>= yamlScreenWidth))
    , screenHeight =
        fromMaybe
          (screenHeight defaultConfig)
          (cliScreenHeight cliCfg <|> (yamlCfgOpt >>= yamlScreenHeight))
    , fullscreenDisplay =
        (cliFullscreenDisplay cliCfg <|> (yamlCfgOpt >>= yamlFullscreenDisplay))
    , debug = (cliDebug cliCfg || (fromMaybe False $ yamlDebug <$> yamlCfgOpt))
    , fontFilePath = yamlCfgOpt >>= yamlFontFilePath
    , fontSize =
        fromMaybe (fontSize defaultConfig) (yamlCfgOpt >>= yamlFontSize)
    , textureDirectories =
        fromMaybe (textureDirectories defaultConfig) $
        yamlTextureDirectories <$> yamlCfgOpt
    }
