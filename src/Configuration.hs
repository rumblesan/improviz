module Configuration
  ( ImpConfig(..)
  , ImpFontConfig(..)
  , getConfig
  ) where

import           Control.Applicative       ((<|>))
import           Data.Maybe                (fromMaybe)
import           Options.Applicative       (execParser)

import           Configuration.CLIConfig   (ImpCLIConfig (..), cliopts,
                                            cliparser)
import           Configuration.YamlConfig  (ImpYAMLConfig (..),
                                            ImpYAMLFontConfig (..),
                                            readConfigFile)

import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

data ImpFontConfig = ImpFontConfig
  { fontFilePath :: Maybe FilePath
  , fontSize     :: Int
  , fontFGColour :: Color4 GLfloat
  , fontBGColour :: Color4 GLfloat
  } deriving (Show)

data ImpConfig = ImpConfig
  { screenWidth        :: Int
  , screenHeight       :: Int
  , fullscreenDisplay  :: Maybe Int
  , debug              :: Bool
  , fontConfig         :: ImpFontConfig
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
  , fontConfig =
      ImpFontConfig
      { fontFilePath = Nothing
      , fontSize = 36
      , fontFGColour = Color4 0.0 0.0 0.0 1.0
      , fontBGColour = Color4 1.0 0.8 0.0 1.0
      }
  , textureDirectories = ["./textures"]
  }

getFontConfig :: ImpFontConfig -> Maybe ImpYAMLFontConfig -> ImpFontConfig
getFontConfig defaultFontCfg yamlFontCfg =
  ImpFontConfig
  { fontFilePath =
      (yamlFontCfg >>= yamlFontFilePath) <|> fontFilePath defaultFontCfg
  , fontSize =
      fromMaybe (fontSize defaultFontCfg) (yamlFontCfg >>= yamlFontSize)
  , fontFGColour =
      fromMaybe (fontFGColour defaultFontCfg) (yamlFontCfg >>= yamlFontFGColour)
  , fontBGColour =
      fromMaybe (fontBGColour defaultFontCfg) (yamlFontCfg >>= yamlFontBGColour)
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
        cliFullscreenDisplay cliCfg <|> (yamlCfgOpt >>= yamlFullscreenDisplay)
    , debug = cliDebug cliCfg || maybe False yamlDebug yamlCfgOpt
    , fontConfig =
        getFontConfig (fontConfig defaultConfig) (yamlFontCfg <$> yamlCfgOpt)
    , textureDirectories =
        maybe
          (textureDirectories defaultConfig)
          yamlTextureDirectories
          yamlCfgOpt
    }
