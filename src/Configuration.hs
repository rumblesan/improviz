{-# LANGUAGE TemplateHaskell #-}

module Configuration
  ( ImprovizConfig
  , screenWidth
  , screenHeight
  , fullscreenDisplay
  , debug
  , fontConfig
  , textureDirectories
  , serverPort
  , fontFilePath
  , fontSize
  , fontFGColour
  , fontBGColour
  , ImprovizFontConfig
  , getConfig
  ) where

import           Control.Applicative       ((<|>))
import           Data.Maybe                (fromMaybe)
import           Options.Applicative       (execParser)

import           Configuration.CLIConfig   (ImprovizCLIConfig (..), cliopts,
                                            cliparser)
import           Configuration.YamlConfig  (ImprovizYAMLConfig (..),
                                            ImprovizYAMLFontConfig (..),
                                            readConfigFile)

import           Graphics.Rendering.OpenGL (Color4 (..), GLfloat)

import           Lens.Simple

data ImprovizFontConfig = ImprovizFontConfig
  { _fontFilePath :: Maybe FilePath
  , _fontSize     :: Int
  , _fontFGColour :: Color4 GLfloat
  , _fontBGColour :: Color4 GLfloat
  } deriving (Show)

makeLenses ''ImprovizFontConfig

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
  , _fontConfig =
      ImprovizFontConfig
      { _fontFilePath = Nothing
      , _fontSize = 36
      , _fontFGColour = Color4 0.0 0.0 0.0 1.0
      , _fontBGColour = Color4 1.0 0.8 0.0 1.0
      }
  , _textureDirectories = ["./textures"]
  , _serverPort = 3000
  }

getFontConfig ::
     ImprovizFontConfig -> Maybe ImprovizYAMLFontConfig -> ImprovizFontConfig
getFontConfig defaultFontCfg yamlFontCfg =
  ImprovizFontConfig
  { _fontFilePath =
      (yamlFontCfg >>= yamlFontFilePath) <|> _fontFilePath defaultFontCfg
  , _fontSize =
      fromMaybe (_fontSize defaultFontCfg) (yamlFontCfg >>= yamlFontSize)
  , _fontFGColour =
      fromMaybe
        (_fontFGColour defaultFontCfg)
        (yamlFontCfg >>= yamlFontFGColour)
  , _fontBGColour =
      fromMaybe
        (_fontBGColour defaultFontCfg)
        (yamlFontCfg >>= yamlFontBGColour)
  }

getConfig :: IO ImprovizConfig
getConfig = do
  cliCfg <- execParser cliopts
  yamlCfgOpt <-
    readConfigFile $ fromMaybe defaultConfigFile $ cliConfigFilePath cliCfg
  return
    ImprovizConfig
    { _screenWidth =
        fromMaybe
          (_screenWidth defaultConfig)
          (cliScreenWidth cliCfg <|> (yamlCfgOpt >>= yamlScreenWidth))
    , _screenHeight =
        fromMaybe
          (_screenHeight defaultConfig)
          (cliScreenHeight cliCfg <|> (yamlCfgOpt >>= yamlScreenHeight))
    , _fullscreenDisplay =
        cliFullscreenDisplay cliCfg <|> (yamlCfgOpt >>= yamlFullscreenDisplay)
    , _debug = cliDebug cliCfg || maybe False yamlDebug yamlCfgOpt
    , _fontConfig =
        getFontConfig (_fontConfig defaultConfig) (yamlFontCfg <$> yamlCfgOpt)
    , _textureDirectories =
        maybe
          (_textureDirectories defaultConfig)
          yamlTextureDirectories
          yamlCfgOpt
    , _serverPort =
        fromMaybe (_serverPort defaultConfig) (yamlCfgOpt >>= yamlServerPort)
    }
