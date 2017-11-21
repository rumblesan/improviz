{-# LANGUAGE OverloadedStrings #-}

module Configuration
  ( ImpConfig(..)
  , getConfig
  ) where

import           Control.Applicative
import qualified Data.ByteString     as B
import           Data.Maybe          (fromMaybe)
import           Data.Semigroup      ((<>))
import           Options.Applicative

import           Data.Yaml           (FromJSON (..), (.:), (.:?), (.!=))
import qualified Data.Yaml           as Y

data ImpCLIConfig = ImpCLIConfig
  { cliScreenWidth       :: Maybe Int
  , cliScreenHeight      :: Maybe Int
  , cliFullscreenDisplay :: Maybe Int
  , cliDebug :: Bool
  , cliConfigFilePath    :: Maybe FilePath
  }

data ImpYAMLConfig = ImpYAMLConfig
  { yamlScreenWidth :: Maybe Int
  , yamlScreenHeight :: Maybe Int
  , yamlFullscreenDisplay :: Maybe Int
  , yamlDebug :: Bool
  , yamlFontFilePath       :: Maybe FilePath
  , yamlTextureDirectories :: [FilePath]
  }

instance FromJSON ImpYAMLConfig where
  parseJSON (Y.Object v) =
    ImpYAMLConfig <$>
      v .:? "screenwidth" <*>
      v .:? "screenheight" <*>
      v .:? "fullscreen" <*>
      v .:? "debug" .!= False <*>
      v .:? "fontfile" <*>
      v .:? "textureDirectories" .!= []
  parseJSON _ = fail "Expected Object for Config value"

data ImpConfig = ImpConfig
  { screenWidth        :: Int
  , screenHeight       :: Int
  , fullscreenDisplay  :: Maybe Int
  , debug :: Bool
  , fontFilePath       :: Maybe FilePath
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
  , textureDirectories = []
  }

defaultWidth :: Int
defaultWidth = 640

defaultHeight :: Int
defaultHeight = 480

defaultConfigFile :: FilePath
defaultConfigFile = "./improviz.yaml"

cliparser :: Parser ImpCLIConfig
cliparser =
  ImpCLIConfig <$>
  optional
    (option
       auto
       (long "width" <> short 'w' <> help "Screen width" <> metavar "INT")) <*>
  optional
    (option
       auto
       (long "height" <> short 'h' <> help "Screen height" <> metavar "INT")) <*>
  optional
    (option
       auto
       (long "fullscreen" <> short 'f' <>
        help "Which screen to fullscreen the app to" <>
        metavar "INT")) <*>
    switch
       (long "debug" <> short 'd' <>
        help "Put improviz in debug mode"
        ) <*>
  optional
    (option
       auto
       (long "config" <> short 'c' <> help "Path to a configuration YAML file" <>
        metavar "FilePath"))

readConfigFile :: ImpCLIConfig -> IO ImpConfig
readConfigFile cliCfg = do
  yaml <-
    Y.decodeFileEither $ fromMaybe defaultConfigFile $ cliConfigFilePath cliCfg
  case yaml of
    Left err -> print err >> return defaultConfig
    Right yamlConfig ->
      return $ ImpConfig
      { screenWidth = fromMaybe defaultWidth (cliScreenWidth cliCfg <|> yamlScreenWidth yamlConfig)
      , screenHeight = fromMaybe defaultHeight (cliScreenHeight cliCfg <|> yamlScreenHeight yamlConfig)
      , fullscreenDisplay = (cliFullscreenDisplay cliCfg <|> yamlFullscreenDisplay yamlConfig)
      , debug = (cliDebug cliCfg || yamlDebug yamlConfig)
      , fontFilePath = yamlFontFilePath yamlConfig
      , textureDirectories = yamlTextureDirectories yamlConfig
      }

getConfig :: (ImpConfig -> IO ()) -> IO ()
getConfig app = execParser opts >>= readConfigFile >>= app
  where
    opts =
      info
        (cliparser <**> helper)
        (fullDesc <> progDesc "Visual live coding environment" <>
         header "Improviz")
