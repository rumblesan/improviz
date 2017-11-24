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

defaultConfig :: ImpConfig
defaultConfig =
  ImpConfig
  { screenWidth = defaultWidth
  , screenHeight = defaultHeight
  , fullscreenDisplay = Nothing
  , debug = False
  , fontFilePath = Nothing
  , fontSize = defaultFontSize
  , textureDirectories = []
  }

defaultWidth :: Int
defaultWidth = 640

defaultHeight :: Int
defaultHeight = 480

defaultConfigFile :: FilePath
defaultConfigFile = "./improviz.yaml"

defaultFontSize :: Int
defaultFontSize = 36

getConfig :: IO ImpConfig
getConfig = do
  cliCfg <- execParser cliopts
  yamlCfgOpt <-
    readConfigFile $ fromMaybe defaultConfigFile $ cliConfigFilePath cliCfg
  return
    ImpConfig
    { screenWidth =
        fromMaybe
          defaultWidth
          (cliScreenWidth cliCfg <|> (yamlCfgOpt >>= yamlScreenWidth))
    , screenHeight =
        fromMaybe
          defaultHeight
          (cliScreenHeight cliCfg <|> (yamlCfgOpt >>= yamlScreenHeight))
    , fullscreenDisplay =
        (cliFullscreenDisplay cliCfg <|> (yamlCfgOpt >>= yamlFullscreenDisplay))
    , debug = (cliDebug cliCfg || (fromMaybe False $ yamlDebug <$> yamlCfgOpt))
    , fontFilePath = yamlCfgOpt >>= yamlFontFilePath
    , fontSize = fromMaybe defaultFontSize (yamlCfgOpt >>= yamlFontSize)
    , textureDirectories = fromMaybe [] $ yamlTextureDirectories <$> yamlCfgOpt
    }
